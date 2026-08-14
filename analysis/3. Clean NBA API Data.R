
# 3. Clean NBA API Data
# Cleans everything scraped off the NBA API - schedule, player game logs, player bio,
# career stats and in-game tracking load - and stacks the five seasons into one clean
# file per dataset for the panel to pick up.
# Input:   data/raw/{nba_schedule, nba_player_travel, nba_game_load}/
# Output:  data/processed/clean_{sched,game_logs,player_bio,career,game_load}.rds
#          data/processed/preseason_participation.rds

library(tidyverse)
library(arrow)

SEASONS <- c("2021-22", "2022-23", "2023-24", "2024-25", "2025-26")
RAW_DIR <- here::here("data", "raw")
OUT_DIR <- here::here("data", "processed")
dir.create(OUT_DIR, showWarnings = FALSE)

message("=== NBA Data Processing Pipeline ===")

# Games a team was scheduled to play. Needed so career games missed is right in the
# lockout / COVID seasons instead of assuming 82.
SCHEDULED_GAMES <- c("1998-99" = 50L, "2011-12" = 66L, "2019-20" = 72L, "2020-21" = 72L)
scheduled_for <- function(season_id) as.integer(coalesce(SCHEDULED_GAMES[season_id], 82L))

read_raw <- function(dir, file) read_csv(file.path(RAW_DIR, dir, file), show_col_types = FALSE)

# Road trips: consecutive away games, then number each game within
# a trip and accumulate its travel.
add_road_trip_cols <- function(df) {
  df %>%
    mutate(
      away_run_id = { r <- rle(IS_AWAY == 1); rep(seq_along(r$lengths) * r$values, r$lengths) },
      TEAM_IS_ROAD_TRIP = as.integer(away_run_id > 0),
      TEAM_ROAD_TRIP_GAME_NUMBER = if_else(away_run_id > 0,
                                           ave(away_run_id, away_run_id, FUN = seq_along), 0L),
      TEAM_ROAD_TRIP_LENGTH = if_else(away_run_id > 0,
                                      ave(away_run_id, away_run_id, FUN = length), 0L),
      TEAM_CUMULATIVE_ROAD_TRIP_KM =
        ave(replace_na(TEAM_TRAVEL_KM, 0) * (away_run_id > 0), away_run_id, FUN = cumsum)
    ) %>%
    select(-away_run_id)
}

# ── Schedule ── regular season only (game-ID prefix 002), plus road trips
message("\n[1] Schedule ...")

clean_sched <- map_dfr(SEASONS, function(s) {
  read_raw("nba_schedule", paste0("nba_schedule_", s, ".csv")) %>%
    filter(str_sub(as.character(gameId), 1, 3) == "002") %>%
    mutate(SEASON = s, GAME_DATE = as.Date(gameDate),
           GAME_ID = as.character(gameId), TEAM_ID = as.integer(TEAM_ID)) %>%
    rename(TEAM_DAYS_REST = DAYS_REST, TEAM_IS_B2B = IS_BACK_TO_BACK,
           TEAM_TRAVEL_KM = TRAVEL_DISTANCE_KM, TEAM_IS_LATE_GAME = IS_LATE_GAME) %>%
    # The schedule files carry team box-score columns; they have to go or the team
    # MIN/PTS collide with the player's when the panel is joined.
    select(-any_of(c("WL", "MIN", "PTS", "FGM", "FGA", "FG_PCT", "FG3M", "FG3A",
                     "FG3_PCT", "FTM", "FTA", "FT_PCT", "OREB", "DREB", "REB", "AST",
                     "STL", "BLK", "TOV", "PF", "PLUS_MINUS"))) %>%
    arrange(TEAM_ID, GAME_DATE) %>%
    group_by(TEAM_ID) %>% group_modify(~ add_road_trip_cols(.x)) %>% ungroup()
})

# Opponent rest / congestion, by joining the table to itself on the same game.
clean_sched <- clean_sched %>%
  left_join(clean_sched %>%
              select(GAME_ID, TEAM_TRICODE,
                     OPPONENT_DAYS_REST    = TEAM_DAYS_REST,
                     OPPONENT_IS_B2B       = TEAM_IS_B2B,
                     OPPONENT_IS_ROAD_TRIP = TEAM_IS_ROAD_TRIP) %>%
              distinct(GAME_ID, TEAM_TRICODE, .keep_all = TRUE),
            by = c("GAME_ID", "OPPONENT_TRICODE" = "TEAM_TRICODE"))

message("  ", nrow(clean_sched), " team-game rows")

# ── Player game logs ── box scores
message("\n[2] Game logs ...")

clean_game_logs <- map_dfr(SEASONS, function(s) {
  read_raw("nba_player_travel", paste0("game_logs_", s, ".csv")) %>%
    mutate(SEASON = s, GAME_DATE = as.Date(GAME_DATE), GAME_ID = as.character(GAME_ID),
           PLAYER_ID = as.integer(PLAYER_ID), TEAM_ID = as.integer(TEAM_ID)) %>%
    select(-any_of(c("NBA_FANTASY_PTS", "WNBA_FANTASY_PTS", "DD2", "TD3", "GP_RANK",
                     "AVAILABLE_FLAG", "MIN_SEC", "TEAM_COUNT", "NICKNAME")),
           -matches("_RANK$")) %>%
    arrange(PLAYER_ID, GAME_DATE)
})

message("  ", nrow(clean_game_logs), " rows | ",
        n_distinct(clean_game_logs$PLAYER_ID), " players")

# Pre-season appearances, my training-camp proxy. Derived here so the panel's
# PLAYED_PRESEASON traces back to the raw game logs.
preseason_participation <- clean_game_logs %>%
  filter(IS_PRESEASON == 1) %>%
  count(SEASON, PLAYER_ID, name = "PRESEASON_GP")

# ── Career stats ── a pre-season snapshot, so every career covariate is lagged
message("\n[3] Career stats ...")

clean_career <- map_dfr(SEASONS, function(s) {
  raw <- read_raw("nba_player_travel", paste0("player_career_stats_", s, ".csv")) %>%
    mutate(PLAYER_ID = as.integer(PLAYER_ID))
  prior_season_id <- paste0(as.integer(str_sub(s, 1, 4)) - 1, "-", str_sub(s, 3, 4))

  # Prior season. Prefer the "TOT" row so a traded player gets their whole season.
  prior <- raw %>%
    filter(SEASON_ID == prior_season_id) %>%
    group_by(PLAYER_ID) %>%
    filter(TEAM_ABBREVIATION == "TOT" | !any(TEAM_ABBREVIATION == "TOT")) %>%
    ungroup() %>%
    select(PLAYER_ID, PRIOR_SEASON_GP = GP, PRIOR_SEASON_MIN_PG = MIN,
           PRIOR_SEASON_PTS = PTS) %>%
    distinct(PLAYER_ID, .keep_all = TRUE)

  # Career totals over every season BEFORE this one, so they are already lagged.
  # Games missed sums GP per player-season first, so a traded player's split stints
  # are not counted as two separate seasons of absence.
  seasons_only <- raw %>% filter(TEAM_ABBREVIATION != "TOT")

  totals <- seasons_only %>%
    group_by(PLAYER_ID) %>%
    summarise(CAREER_GAMES      = sum(GP, na.rm = TRUE),
              CAREER_MINUTES    = sum(MIN * GP, na.rm = TRUE),
              SEASONS_IN_LEAGUE = n_distinct(SEASON_ID), .groups = "drop")

  missed <- seasons_only %>%
    group_by(PLAYER_ID, SEASON_ID) %>%
    summarise(gp = sum(GP, na.rm = TRUE), .groups = "drop") %>%
    group_by(PLAYER_ID) %>%
    summarise(CAREER_GAMES_MISSED = sum(pmax(0L, scheduled_for(SEASON_ID) - as.integer(gp))),
              .groups = "drop")

  prior %>%
    full_join(totals, by = "PLAYER_ID") %>%
    left_join(missed, by = "PLAYER_ID") %>%
    mutate(SEASON = s)
})

message("  ", nrow(clean_career), " player-season rows | ",
        sum(is.na(clean_career$PRIOR_SEASON_GP)), " rookies")

# ── In-game tracking load ──
message("\n[4] Game load ...")

clean_game_load <- map_dfr(SEASONS, function(s) {
  read_parquet(file.path(RAW_DIR, "nba_game_load", paste0("nba_game_load_", s, ".parquet"))) %>%
    mutate(SEASON = s, PLAYER_ID = as.integer(PLAYER_ID), GAME_ID = as.character(GAME_ID))
})

message("  ", nrow(clean_game_load), " rows")

# ── Player bio ── one file for every player, not per season
clean_player_bio <- read_raw("nba_player_travel", "player_bio.csv") %>%
  mutate(PLAYER_ID = as.integer(PLAYER_ID),
         BIRTHDATE = as.Date(BIRTHDATE, format = "%Y-%m-%d"),
         # Height arrives as feet-inches text like "6-9".
         HEIGHT_INCHES = 12L * as.integer(str_extract(HEIGHT, "^\\d+")) +
                               as.integer(str_extract(HEIGHT, "\\d+$"))) %>%
  select(PLAYER_ID, DISPLAY_FIRST_LAST, BIRTHDATE, POSITION, HEIGHT, HEIGHT_INCHES,
         WEIGHT, COUNTRY, SCHOOL, DRAFT_YEAR, DRAFT_ROUND, DRAFT_NUMBER, SEASON_EXP) %>%
  distinct(PLAYER_ID, .keep_all = TRUE)

# ── Save ──
saveRDS(clean_sched,             file.path(OUT_DIR, "clean_sched.rds"))
saveRDS(clean_game_logs,         file.path(OUT_DIR, "clean_game_logs.rds"))
saveRDS(clean_player_bio,        file.path(OUT_DIR, "clean_player_bio.rds"))
saveRDS(clean_career,            file.path(OUT_DIR, "clean_career.rds"))
saveRDS(clean_game_load,         file.path(OUT_DIR, "clean_game_load.rds"))
saveRDS(preseason_participation, file.path(OUT_DIR, "preseason_participation.rds"))

message("\n=== Cleaning complete === ", nrow(clean_player_bio), " players in bio")

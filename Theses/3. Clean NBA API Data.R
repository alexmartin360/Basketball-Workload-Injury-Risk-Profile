# =============================================================================
# 3. Clean NBA API Data
# =============================================================================
# This is my cleaning stage for everything I scraped off the NBA API — the
# schedule, the player game logs, rosters, player bio, career stats and the
# in-game tracking load. I loop over my five seasons, clean each dataset the same
# way every season, derive the schedule-stress features I care about (rest days,
# back-to-backs, road trips) and the lagged career context, then stack the seasons
# into one clean file per dataset for the panel to pick up.
# Input:   nba_schedule/*.csv, nba_player_travel/*.csv (game logs, rosters,
#          player_bio, career stats), nba_game_load/*.parquet
# Output:  data_processed/clean_{sched,game_logs,rosters,player_bio,career,game_load}.rds
# =============================================================================

library(tidyverse)
library(lubridate)
library(slider)
library(arrow)
library(fs)

# =============================================================================
# SECTION 1: CONFIG & HELPERS
# =============================================================================

# Injury reports available from 2021-22 onward — this is the modelling window.
# Career stats snapshots go back to 2015-16 for lagged career context.
SEASONS <- c("2021-22", "2022-23", "2023-24", "2024-25", "2025-26")

BASE_PATH <- "/Users/alexmartin/Library/Mobile Documents/com~apple~CloudDocs/Filing Cabinet/University/Latrobe/SPA-A/Project Code Base/Basketball-Workload-Injury-Risk-Profile"
OUT_DIR   <- file.path(BASE_PATH, "data_processed")
dir.create(OUT_DIR, showWarnings = FALSE)

message("=== NBA Data Processing Pipeline ===")
message("    Seasons: ", paste(SEASONS, collapse = ", "))

# How many regular-season games each team is scheduled to play, by season. It's 82
# in a normal year, but I need the reduced seasons right or my "career games missed"
# count comes out wrong — a lockout/COVID season isn't 82 games, so I'd over-count
# the games a player "missed". The named seasons below are the lockout/COVID ones.
# (2019-20 teams actually played 64-75; I use 72 as a league-target approximation.)
SCHEDULED_GAMES <- c(
  "1998-99" = 50L,   # lockout
  "2011-12" = 66L,   # lockout
  "2019-20" = 72L,   # COVID (approx — teams varied 64-75)
  "2020-21" = 72L    # COVID (uniform 72)
)
scheduled_for <- function(season_id) {
  out <- SCHEDULED_GAMES[season_id]
  as.integer(ifelse(is.na(out), 82L, out))
}

# Small helper I use everywhere below — read a season's CSV, but if the file isn't
# there just warn me and return NULL rather than crashing the whole run.
read_season_csv <- function(path, season, label) {
  if (!file.exists(path)) {
    warning(sprintf("[%s] %s not found: %s — skipping season.", season, label, path))
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE)
}

# Helper — I work out road-trip indicators from a team's schedule (already sorted).
# The idea: run-length-encode the consecutive away games, then number each game
# within a trip and record the trip length, and zero both out whenever the team
# isn't actually on a road trip.
add_road_trip_cols <- function(df) {
  df %>%
    mutate(
      away_run_id = {
        away <- IS_AWAY == 1
        rle_ <- rle(away)
        rep(seq_along(rle_$lengths) * rle_$values, rle_$lengths)
      },
      TEAM_ROAD_TRIP_GAME_NUMBER = if_else(
        away_run_id > 0,
        ave(away_run_id, away_run_id, FUN = seq_along), 0L
      ),
      TEAM_ROAD_TRIP_LENGTH = if_else(
        away_run_id > 0,
        ave(away_run_id, away_run_id, FUN = length), 0L
      ),
      TEAM_IS_ROAD_TRIP = as.integer(
        away_run_id > 0 & TEAM_ROAD_TRIP_LENGTH >= 1
      ),
      TEAM_ROAD_TRIP_GAME_NUMBER = if_else(
        TEAM_IS_ROAD_TRIP == 1, TEAM_ROAD_TRIP_GAME_NUMBER, 0L
      ),
      TEAM_ROAD_TRIP_LENGTH = if_else(
        TEAM_IS_ROAD_TRIP == 1, TEAM_ROAD_TRIP_LENGTH, 0L
      ),
      TEAM_CUMULATIVE_ROAD_TRIP_KM = {
        km <- if_else(is.na(TEAM_TRAVEL_KM), 0, TEAM_TRAVEL_KM)
        ave(km * (away_run_id > 0), away_run_id, FUN = cumsum) *
          (TEAM_IS_ROAD_TRIP == 1)
      }
    ) %>%
    select(-away_run_id)
}

# =============================================================================
# SECTION 2: CLEAN / PREPARE — loop over seasons
# =============================================================================

message("\n--- SECTION 2: Clean / Prepare ---")

# Accumulators — one list per dataset, one element per season
sched_list      <- list()
game_logs_list  <- list()
rosters_list    <- list()
career_list     <- list()
injuries_list   <- list()
game_load_list  <- list()

for (SEASON in SEASONS) {
  
  message("\n", strrep("=", 60))
  message("  Processing season: ", SEASON)
  message(strrep("=", 60))
  
  # Prior season ID for the career stats lagged join
  prior_season_id <- paste0(
    as.integer(str_sub(SEASON, 1, 4)) - 1,
    "-",
    str_sub(SEASON, 3, 4)
  )
  
  # ── 2a. Schedule ── clean schedule, add road trips + opponent context
  raw_schedule <- read_season_csv(
    file.path(BASE_PATH, "nba_schedule", paste0("nba_schedule_", SEASON, ".csv")),
    SEASON, "schedule"
  )
  
  # I keep only REGULAR-SEASON games, and I do it off the game-ID prefix:
  #   001 = pre-season, 002 = regular, 003 = all-star, 004 = playoffs,
  #   005 = play-in, 006 = NBA Cup final.
  # Filtering to "002" drops every non-regular game type in one go, which I found
  # far more reliable than trying to match the gameSubtype/label strings.
  #
  # One thing I'm deliberately leaving out (worth revisiting later): the NBA Cup
  # final (prefix 006) is a real game the two finalists play, but my game-log
  # scraper only pulls regular-season logs. So the Cup final would show up as a
  # PLAYED_FLAG = 0 phantom with carried-forward covariates, AND it's an 83rd game
  # that breaks the 82-game assumption my scheduling / games-missed logic leans on.
  # So I exclude it here. If I ever want it properly, I'd fetch the Cup final game
  # log as a real played game first, then relax this filter.
  if (!is.null(raw_schedule)) {
    n_before     <- nrow(raw_schedule)
    raw_schedule <- raw_schedule %>%
      filter(str_sub(as.character(gameId), 1, 3) == "002")
    n_dropped <- n_before - nrow(raw_schedule)
    if (n_dropped > 0) {
      message("  [2a] schedule: dropped ", n_dropped,
              " non-regular-season games (preseason/playoffs/playin/cup)")
    }
  }
  
  if (!is.null(raw_schedule)) {
    sched_season <- raw_schedule %>%
      mutate(
        SEASON    = SEASON,
        GAME_DATE = as.Date(gameDate),
        GAME_ID   = as.character(gameId),
        TEAM_ID   = as.integer(TEAM_ID),
      ) %>%
      rename(
        TEAM_DAYS_REST    = DAYS_REST,
        TEAM_IS_B2B       = IS_BACK_TO_BACK,
        TEAM_TRAVEL_KM    = TRAVEL_DISTANCE_KM,
        TEAM_IS_LATE_GAME = IS_LATE_GAME,
      ) %>%
      # Drop the team box-score stats if an OLD schedule CSV still carries them.
      # nba_schedule.py no longer scrapes these (nothing downstream used them), but
      # this keeps the clean step correct for pre-existing files — and stops the
      # raw team MIN colliding with the player MIN when the panel is joined.
      select(-any_of(c(
        "WL", "MIN", "PTS", "FGM", "FGA", "FG_PCT", "FG3M", "FG3A", "FG3_PCT",
        "FTM", "FTA", "FT_PCT", "OREB", "DREB", "REB", "AST", "STL", "BLK",
        "TOV", "PF", "PLUS_MINUS"
      ))) %>%
      arrange(TEAM_ID, GAME_DATE) %>%
      group_by(TEAM_ID) %>%
      group_modify(~ add_road_trip_cols(.x)) %>%
      ungroup() %>%
      # I also want each team's OPPONENT's rest/B2B/road-trip situation, so I join the
      # table onto itself on GAME_ID, matching my OPPONENT_TRICODE to the other row's
      # TEAM_TRICODE. That gives me "how tired was the team I'm playing" for free.
      left_join(
        (.) %>%
          select(
            GAME_ID, TEAM_TRICODE,
            OPPONENT_DAYS_REST    = TEAM_DAYS_REST,
            OPPONENT_IS_B2B       = TEAM_IS_B2B,
            OPPONENT_IS_ROAD_TRIP = TEAM_IS_ROAD_TRIP,
          ) %>%
          distinct(GAME_ID, TEAM_TRICODE, .keep_all = TRUE),
        by = c("GAME_ID", "OPPONENT_TRICODE" = "TEAM_TRICODE")
      )
    
    sched_list[[SEASON]] <- sched_season
    message("  [2a] schedule: ", nrow(sched_season), " rows")
  }
  
  # ── 2b. Game logs ── clean player box scores, drop rank/fantasy noise
  raw_game_logs <- read_season_csv(
    file.path(BASE_PATH, "nba_player_travel", paste0("game_logs_", SEASON, ".csv")),
    SEASON, "game_logs"
  )
  
  if (!is.null(raw_game_logs)) {
    logs_season <- raw_game_logs %>%
      mutate(
        SEASON    = SEASON,
        GAME_DATE = as.Date(GAME_DATE),
        GAME_ID   = as.character(GAME_ID),
        PLAYER_ID = as.integer(PLAYER_ID),
        TEAM_ID   = as.integer(TEAM_ID),
      ) %>%
      select(-any_of(c(
        "NBA_FANTASY_PTS", "WNBA_FANTASY_PTS", "DD2", "TD3",
        "GP_RANK", "AVAILABLE_FLAG", "MIN_SEC", "TEAM_COUNT", "NICKNAME"
      ))) %>%
      select(-matches("_RANK$")) %>%
      arrange(PLAYER_ID, GAME_DATE)
    
    game_logs_list[[SEASON]] <- logs_season
    message("  [2b] game_logs: ", nrow(logs_season), " rows | ",
            n_distinct(logs_season$PLAYER_ID), " players")
  }
  
  # ── 2c. Rosters ── clean season roster, de-duplicate player-team rows
  raw_rosters <- read_season_csv(
    file.path(BASE_PATH, "nba_player_travel", paste0("rosters_", SEASON, ".csv")),
    SEASON, "rosters"
  )
  
  if (!is.null(raw_rosters)) {
    rosters_season <- raw_rosters %>%
      mutate(
        SEASON    = SEASON,
        PLAYER_ID = as.integer(PLAYER_ID),
        TEAM_ID   = as.integer(TEAM_ID),
      ) %>%
      distinct(PLAYER_ID, TEAM_ID, .keep_all = TRUE)
    
    rosters_list[[SEASON]] <- rosters_season
    message("  [2c] rosters: ", nrow(rosters_season), " rows | ",
            n_distinct(rosters_season$PLAYER_ID), " players")
  }
  
  # ── 2e. Career stats ── prior-season snapshot (lagged) + career totals per player
  raw_career <- read_season_csv(
    file.path(BASE_PATH, "nba_player_travel",
              paste0("player_career_stats_", SEASON, ".csv")),
    SEASON, "career_stats"
  )
  
  if (!is.null(raw_career)) {
    raw_career <- raw_career %>%
      mutate(PLAYER_ID = as.integer(PLAYER_ID))
    
    # Prior-season stats — the season right before this one. For a player who was
    # traded mid-season I prefer the "TOT" (total) row so I get their full season,
    # not just one team's slice.
    prior_stats <- raw_career %>%
      filter(SEASON_ID == prior_season_id) %>%
      group_by(PLAYER_ID) %>%
      mutate(has_tot = any(TEAM_ABBREVIATION == "TOT")) %>%
      filter((has_tot & TEAM_ABBREVIATION == "TOT") | !has_tot) %>%
      mutate(
        primary_team = if_else(
          TEAM_ABBREVIATION == "TOT",
          {
            non_tot <- cur_data() %>% filter(TEAM_ABBREVIATION != "TOT")
            if (nrow(non_tot) > 0) {
              non_tot %>%
                slice_max(GP, n = 1, with_ties = FALSE) %>%
                pull(TEAM_ABBREVIATION)
            } else {
              TEAM_ABBREVIATION
            }
          },
          TEAM_ABBREVIATION
        )
      ) %>%
      ungroup() %>%
      mutate(TEAM_ABBREVIATION = primary_team) %>%
      select(
        PLAYER_ID,
        PRIOR_TEAM          = TEAM_ABBREVIATION,
        PRIOR_SEASON_GP     = GP,
        PRIOR_SEASON_MIN_PG = MIN,
        PRIOR_SEASON_PTS    = PTS,
        PRIOR_SEASON_REB    = REB,
        PRIOR_SEASON_AST    = AST,
        any_of(c(PRIOR_SEASON_USG = "USG_PCT", PRIOR_SEASON_TS = "TS_PCT"))
      ) %>%
      distinct(PLAYER_ID, .keep_all = TRUE)
    
    # Career totals — I aggregate across all of a player's prior seasons.
    # For CAREER_GAMES_MISSED I sum (scheduled - GP) over every prior season in the
    # snapshot. I first sum GP per player-season (a traded player has several team
    # rows in one season) before subtracting that season's scheduled count, so a
    # partial stint doesn't get double-counted. Because the snapshot only ever
    # holds seasons BEFORE the one I'm modelling, this is already lagged — which is
    # exactly what I want: a clean, exogenous durability signal (this is the
    # cumulative-burden idea from Martin 2021).
    season_missed <- raw_career %>%
      filter(TEAM_ABBREVIATION != "TOT") %>%
      group_by(PLAYER_ID, SEASON_ID) %>%
      summarise(SEASON_GP = sum(GP, na.rm = TRUE), .groups = "drop") %>%
      mutate(SEASON_GAMES_MISSED = pmax(0L, scheduled_for(SEASON_ID) - as.integer(SEASON_GP)))
    
    career_missed <- season_missed %>%
      group_by(PLAYER_ID) %>%
      summarise(CAREER_GAMES_MISSED = sum(SEASON_GAMES_MISSED, na.rm = TRUE), .groups = "drop")
    
    career_totals <- raw_career %>%
      filter(TEAM_ABBREVIATION != "TOT") %>%
      group_by(PLAYER_ID) %>%
      summarise(
        CAREER_GAMES      = sum(GP,       na.rm = TRUE),
        CAREER_MINUTES    = sum(MIN * GP, na.rm = TRUE),
        SEASONS_IN_LEAGUE = n_distinct(SEASON_ID),
        .groups = "drop"
      ) %>%
      left_join(career_missed, by = "PLAYER_ID")
    
    career_season <- prior_stats %>%
      full_join(career_totals, by = "PLAYER_ID") %>%
      mutate(SEASON = SEASON)
    
    career_list[[SEASON]] <- career_season
    message("  [2e] career: ", nrow(career_season), " players | ",
            sum(!is.na(career_season$PRIOR_SEASON_GP)), " with prior season | ",
            sum(is.na(career_season$PRIOR_SEASON_GP)), " rookies")
  }
  
  # ── 2g. Game load ── read in-game tracking metrics (Parquet)
  game_load_path <- file.path(
    BASE_PATH, "nba_game_load",
    paste0("nba_game_load_", SEASON, ".parquet")
  )
  
  if (file.exists(game_load_path)) {
    load_season <- read_parquet(game_load_path) %>%
      mutate(
        SEASON    = SEASON,
        PLAYER_ID = as.integer(PLAYER_ID),
        GAME_ID   = as.character(GAME_ID),
      )
    game_load_list[[SEASON]] <- load_season
    message("  [2g] game_load: ", nrow(load_season), " rows")
  } else {
    message("  [2g] game_load not found for ", SEASON, " — skipping")
  }
  
} # end season loop

# ── Stack all seasons into combined clean datasets ──
message("\n--- Stacking seasons into combined clean datasets ---")

clean_sched      <- bind_rows(sched_list)
clean_game_logs  <- bind_rows(game_logs_list)
clean_rosters    <- bind_rows(rosters_list)
clean_career     <- bind_rows(career_list)
clean_game_load  <- bind_rows(game_load_list)

# Player bio — single file, not season-specific
clean_player_bio <- read_csv(
  file.path(BASE_PATH, "nba_player_travel", "player_bio.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    PLAYER_ID = as.integer(PLAYER_ID),
    BIRTHDATE = as.Date(BIRTHDATE, format = "%Y-%m-%d"),
    # Height comes through as feet-inches text like "6-9", which I can't model. I
    # convert it to total inches (6*12 + 9 = 81) so it's a plain number I can use as
    # a continuous covariate.
    HEIGHT_INCHES = {
      parts <- str_split_fixed(HEIGHT, "-", 2)
      suppressWarnings(as.integer(parts[, 1]) * 12L + as.integer(parts[, 2]))
    },
  ) %>%
  select(
    PLAYER_ID, DISPLAY_FIRST_LAST, BIRTHDATE,
    POSITION, HEIGHT, HEIGHT_INCHES, WEIGHT, COUNTRY, SCHOOL,
    DRAFT_YEAR, DRAFT_ROUND, DRAFT_NUMBER, SEASON_EXP,
  ) %>%
  distinct(PLAYER_ID, .keep_all = TRUE)

# ── Save combined clean datasets ──
saveRDS(clean_sched,      file.path(OUT_DIR, "clean_sched.rds"))
saveRDS(clean_game_logs,  file.path(OUT_DIR, "clean_game_logs.rds"))
saveRDS(clean_rosters,    file.path(OUT_DIR, "clean_rosters.rds"))
saveRDS(clean_player_bio, file.path(OUT_DIR, "clean_player_bio.rds"))
saveRDS(clean_career,     file.path(OUT_DIR, "clean_career.rds"))
saveRDS(clean_game_load,  file.path(OUT_DIR, "clean_game_load.rds"))

message("\n--- Section 2 complete ---")
message("  clean_sched:      ", nrow(clean_sched),     " rows | ",
        n_distinct(clean_sched$SEASON), " seasons")
message("  clean_game_logs:  ", nrow(clean_game_logs), " rows | ",
        n_distinct(clean_game_logs$PLAYER_ID), " unique players")
message("  clean_rosters:    ", nrow(clean_rosters),   " rows")
message("  clean_player_bio: ", nrow(clean_player_bio)," players")
message("  clean_career:     ", nrow(clean_career),    " player-season rows")
message("  clean_game_load:  ", nrow(clean_game_load), " rows")
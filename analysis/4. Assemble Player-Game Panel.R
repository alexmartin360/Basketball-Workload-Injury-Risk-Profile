
# 4. Assemble Player-Game Panel
# Assemble the cleaned pieces into one row per player per team game, played or missed,
# then derives the rolling workload and travel windows, the schedule-density flags, the
# injury episodes (new injury vs reaggravation) and the season-structure context.
# Sources: 1. Injury Classification Rules.R
# Input:   data/processed/clean_*.rds, preseason_participation.rds
# Output:  data/processed/panel_2021-26.rds

library(tidyverse)
library(lubridate)
library(slider)

BASE <- here::here("data", "processed")

# How many games a player must play after an injury before a repeat at the SAME site
# counts as a new episode rather than the old one flaring up. Eight games is roughly a
# fortnight of a team's schedule, the window Heiderscheit et al. (2010) identify as
# carrying the highest re-injury risk.
RETURN_DURABILITY_GAMES <- 8
RECENT_RETURN_DAYS      <- 14L

source(here::here("analysis", "1. Injury Classification Rules.R"))

clean_sched      <- read_rds(file.path(BASE, "clean_sched.rds"))
clean_game_logs  <- read_rds(file.path(BASE, "clean_game_logs.rds"))
clean_player_bio <- read_rds(file.path(BASE, "clean_player_bio.rds"))
clean_career     <- read_rds(file.path(BASE, "clean_career.rds"))
clean_injuries   <- read_rds(file.path(BASE, "clean_injuries.rds"))
clean_game_load  <- read_rds(file.path(BASE, "clean_game_load.rds"))
preseason_participation <- read_rds(file.path(BASE, "preseason_participation.rds"))

message("=== Assembling player-game panel ===")

# Names are the only link between the injury reports and the game logs, so both sides
# the same shape: accents to ASCII, punctuation out, suffixes out.
norm_name <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII") %>% tolower()
  x <- str_replace_all(x, "[.']", "") %>% str_replace_all("-", " ")
  str_squish(str_replace_all(x, "\\b(jr|sr|ii|iii|iv|v)\\b", ""))
}

# ── 1. Panel skeleton ──
# The player universe is everyone who actually APPEARED in a regular-season game, not
# the roster snapshot: that snapshot drops the waived / traded / two-way players and
# with them ~8.5% of genuine injury rows, which is selection bias.
message("\n[1] Building skeleton ...")

gl_reg <- clean_game_logs %>% filter(str_sub(as.character(GAME_ID), 1, 3) == "002")

# Injury dates mapped to PLAYER_ID. Individual dates are kept rather than a span so a
# traded player's between-teams injury lands on the team he was actually on.
inj_dates <- clean_injuries %>%
  filter(IS_INJURY_ABSENCE == 1, IS_OUT == 1) %>%
  mutate(nkey = norm_name(PLAYER_NAME_NORM)) %>%
  inner_join(gl_reg %>% distinct(SEASON, PLAYER_ID, PLAYER_NAME) %>%
               mutate(nkey = norm_name(PLAYER_NAME)),
             by = c("SEASON", "nkey"), relationship = "many-to-many") %>%
  distinct(SEASON, PLAYER_ID, inj_date = game_date)

# Each player-team stint, bounded by the games they played there.
stints <- gl_reg %>%
  group_by(SEASON, PLAYER_ID, TEAM_ID) %>%
  summarise(PLAYER_NAME = first(PLAYER_NAME), TEAM_ABBREVIATION = first(TEAM_ABBREVIATION),
            first_played = min(GAME_DATE), last_played = max(GAME_DATE), .groups = "drop") %>%
  arrange(SEASON, PLAYER_ID, first_played) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(stint_rank = row_number(), n_stints = n(), next_first = dplyr::lead(first_played)) %>%
  ungroup()

# An injury date belongs to the stint whose window contains it: the first stint also
# owns anything before its first game (a season-opening injury), the last stint owns
# everything after, and a middle stint owns up to the next stint's first game.
inj_by_stint <- stints %>%
  left_join(inj_dates, by = c("SEASON", "PLAYER_ID"), relationship = "many-to-many") %>%
  filter(!is.na(inj_date),
         inj_date >= if_else(stint_rank == 1L, as.Date("1900-01-01"), first_played),
         inj_date <  if_else(stint_rank == n_stints, as.Date("2100-01-01"), next_first)) %>%
  group_by(SEASON, PLAYER_ID, TEAM_ID) %>%
  summarise(inj_lo = min(inj_date), inj_hi = max(inj_date), .groups = "drop")

# Tenure window = the games played, to cover the injury dates that stint owns.
# Crossed with the team's schedule, then the box score joined on.
panel <- stints %>%
  left_join(inj_by_stint, by = c("SEASON", "PLAYER_ID", "TEAM_ID")) %>%
  transmute(SEASON, PLAYER_ID, PLAYER_NAME, TEAM_ID, TEAM_ABBREVIATION,
            win_start = pmin(first_played, coalesce(inj_lo, first_played)),
            win_end   = pmax(last_played,  coalesce(inj_hi, last_played))) %>%
  inner_join(clean_sched, by = c("SEASON", "TEAM_ID"), relationship = "many-to-many") %>%
  filter(GAME_DATE >= win_start, GAME_DATE <= win_end) %>%
  select(-win_start, -win_end) %>%
  left_join(gl_reg %>% select(SEASON, PLAYER_ID, GAME_ID, TEAM_ID, MIN, PTS, REB, AST,
                              STL, BLK, TOV, PF, PLUS_MINUS),
            by = c("SEASON", "PLAYER_ID", "GAME_ID", "TEAM_ID")) %>%
  mutate(PLAYED_FLAG = as.integer(!is.na(MIN))) %>%
  arrange(SEASON, PLAYER_ID, GAME_DATE, desc(PLAYED_FLAG)) %>%
  distinct(SEASON, PLAYER_ID, GAME_DATE, .keep_all = TRUE)

stopifnot(nrow(count(panel, SEASON, PLAYER_ID, GAME_DATE) %>% filter(n > 1)) == 0)
message("  ", nrow(panel), " rows | ", n_distinct(panel$PLAYER_ID), " players | ",
        sum(panel$PLAYED_FLAG), " played")

# ── 2. Appearance metrics ── congestion, rolling minutes, road trips
# All computed on games the player actually appeared in, then joined back on.
message("\n[2] Appearance metrics ...")

appeared <- panel %>%
  filter(PLAYED_FLAG == 1) %>%
  arrange(SEASON, PLAYER_ID, GAME_DATE) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(
    PLAYER_DAYS_SINCE_LAST_PLAYED = as.numeric(GAME_DATE - dplyr::lag(GAME_DATE)) - 1,
    IS_B2B     = as.integer(PLAYER_DAYS_SINCE_LAST_PLAYED == 0),
    IS_B2B_2ND = IS_B2B,
    IS_B2B_1ST = as.integer(dplyr::lead(IS_B2B, default = 0L) == 1 & IS_B2B == 0),
    SEASON_B2B_COUNT = cumsum(replace_na(IS_B2B, 0)),
    GAMES_PLAYED     = row_number(),
    IS_3IN4 = as.integer(slide_index_dbl(rep(1, n()), GAME_DATE, sum, .before = days(3)) >= 3),
    IS_4IN5 = as.integer(slide_index_dbl(rep(1, n()), GAME_DATE, sum, .before = days(4)) >= 4),
    IS_5IN7 = as.integer(slide_index_dbl(rep(1, n()), GAME_DATE, sum, .before = days(6)) >= 5),
    ROLLING_MIN_7D   = slide_index_dbl(MIN, GAME_DATE, sum, .before = days(7)),
    ROLLING_MIN_28D  = slide_index_dbl(MIN, GAME_DATE, sum, .before = days(28)),
    ROLLING_MIN_42D  = slide_index_dbl(MIN, GAME_DATE, sum, .before = days(42)),
    SEASON_CUMULATIVE_MIN = cumsum(MIN),
    ROLLING_GAMES_7D  = slide_index_dbl(rep(1, n()), GAME_DATE, sum, .before = days(7)),
    ROLLING_GAMES_28D = slide_index_dbl(rep(1, n()), GAME_DATE, sum, .before = days(28)),
    ROLLING_B2B_7D    = slide_index_dbl(replace_na(IS_B2B, 0L), GAME_DATE, sum, .before = days(7)),
    ROLLING_B2B_28D   = slide_index_dbl(replace_na(IS_B2B, 0L), GAME_DATE, sum, .before = days(28)),
    # Acute:chronic minutes, undefined until there are 4 games in the chronic window.
    ACWR_MIN = if_else(ROLLING_MIN_28D > 0 & ROLLING_GAMES_28D >= 4,
                       round(ROLLING_MIN_7D / (ROLLING_MIN_28D / 4), 3), NA_real_),
    away_run = { r <- rle(IS_AWAY == 1); rep(seq_along(r$lengths) * r$values, r$lengths) },
    # A player road trip needs at least two consecutive away games.
    IS_PLAYER_ROAD_TRIP = as.integer(away_run > 0 & ave(away_run, away_run, FUN = length) >= 2),
    PLAYER_ROAD_TRIP_GAME_NUMBER = if_else(IS_PLAYER_ROAD_TRIP == 1,
                                           ave(away_run, away_run, FUN = seq_along), 0),
    # Travel between arenas, great-circle km from the previous appearance.
    PLAYER_TRAVEL_KM = {
      lat1 <- dplyr::lag(ARENA_LAT) * pi / 180; lat2 <- ARENA_LAT * pi / 180
      dlon <- (ARENA_LON - dplyr::lag(ARENA_LON)) * pi / 180
      a <- sin((lat2 - lat1) / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
      if_else(is.na(lat1) | is.na(lat2), NA_real_, 6371 * 2 * atan2(sqrt(a), sqrt(1 - a)))
    },
    ROLLING_DIST_7D  = slide_index_dbl(replace_na(PLAYER_TRAVEL_KM, 0), GAME_DATE, sum, .before = days(7)),
    ROLLING_DIST_28D = slide_index_dbl(replace_na(PLAYER_TRAVEL_KM, 0), GAME_DATE, sum, .before = days(28)),
    SEASON_CUMULATIVE_DIST = cumsum(replace_na(PLAYER_TRAVEL_KM, 0))
  ) %>%
  ungroup() %>%
  select(SEASON, PLAYER_ID, GAME_ID, PLAYER_DAYS_SINCE_LAST_PLAYED, IS_B2B, IS_B2B_1ST,
         IS_B2B_2ND, SEASON_B2B_COUNT, GAMES_PLAYED, IS_3IN4, IS_4IN5, IS_5IN7,
         ROLLING_MIN_7D, ROLLING_MIN_28D, ROLLING_MIN_42D, SEASON_CUMULATIVE_MIN,
         ROLLING_GAMES_7D, ROLLING_GAMES_28D, ROLLING_B2B_7D, ROLLING_B2B_28D, ACWR_MIN,
         IS_PLAYER_ROAD_TRIP, PLAYER_ROAD_TRIP_GAME_NUMBER, PLAYER_TRAVEL_KM,
         ROLLING_DIST_7D, ROLLING_DIST_28D, SEASON_CUMULATIVE_DIST)

panel <- panel %>% left_join(appeared, by = c("SEASON", "PLAYER_ID", "GAME_ID"))

# ── 3. On-court load ── tracking distance is the external-load exposure
message("\n[3] Tracking load ...")

# clean_game_load has a few duplicate player-games; deduping keeps the join 1:1 
panel <- panel %>%
  left_join(clean_game_load %>%
              select(-any_of(c("TEAMID", "TEAMTRICODE", "MINUTES", "PLUSMINUSPOINTS",
                               "FOULSPERSONAL", "SEASON"))) %>%
              distinct(PLAYER_ID, GAME_ID, .keep_all = TRUE),
            by = c("PLAYER_ID", "GAME_ID")) %>%
  # Tracking metrics are only real on games the player played, otherwise a DNP would
  # carry a fake zero distance through the later carry-forward.
  mutate(across(any_of(c("DISTANCE", "SPEED", "TOUCHES", "USAGEPERCENTAGE",
                         "POSSESSIONS", "PACE")),
                ~ if_else(PLAYED_FLAG == 1, as.numeric(.x), NA_real_)))

tracking_load <- panel %>%
  filter(PLAYED_FLAG == 1) %>%
  arrange(SEASON, PLAYER_ID, GAME_DATE) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(ROLLING_LOAD_7D  = slide_index_dbl(replace_na(DISTANCE, 0), GAME_DATE, sum, .before = days(7)),
         ROLLING_LOAD_28D = slide_index_dbl(replace_na(DISTANCE, 0), GAME_DATE, sum, .before = days(28)),
         ACWR_LOAD = if_else(ROLLING_LOAD_28D > 0 &
                               slide_index_dbl(rep(1, n()), GAME_DATE, sum, .before = days(28)) >= 4,
                             round(ROLLING_LOAD_7D / (ROLLING_LOAD_28D / 4), 3), NA_real_)) %>%
  ungroup() %>%
  select(SEASON, PLAYER_ID, GAME_ID, ROLLING_LOAD_7D, ROLLING_LOAD_28D, ACWR_LOAD)

# Intensity is distance per minute. A rate needs a base of minutes to mean anything, and
# a played game recording zero distance is a tracking-feed failure rather than a real 0.
panel <- panel %>%
  left_join(tracking_load, by = c("SEASON", "PLAYER_ID", "GAME_ID")) %>%
  mutate(INTENSITY = if_else(!is.na(MIN) & MIN >= 10 & !is.na(DISTANCE) & DISTANCE > 0,
                             DISTANCE / MIN, NA_real_),
         ROLLING_INTENSITY_7D  = if_else(ROLLING_MIN_7D  >= 30, ROLLING_LOAD_7D  / ROLLING_MIN_7D,  NA_real_),
         ROLLING_INTENSITY_28D = if_else(ROLLING_MIN_28D >= 60, ROLLING_LOAD_28D / ROLLING_MIN_28D, NA_real_))

# ── 4. Career, bio and pre-season context ──
panel <- panel %>%
  left_join(clean_player_bio %>% select(PLAYER_ID, BIRTHDATE, POSITION, HEIGHT_INCHES,
                                        WEIGHT, DRAFT_YEAR, DRAFT_NUMBER),
            by = "PLAYER_ID") %>%
  left_join(clean_career, by = c("SEASON", "PLAYER_ID")) %>%
  left_join(preseason_participation, by = c("SEASON", "PLAYER_ID")) %>%
  mutate(PLAYED_PRESEASON   = as.integer(replace_na(PRESEASON_GP, 0L) > 0),
         PLAYER_AGE_AT_GAME = round(as.numeric(GAME_DATE - BIRTHDATE) / 365.25, 2)) %>%
  select(-BIRTHDATE)

# ── 5. Injury join ── on the normalised name, or spelling drift silently drops injuries
message("\n[4] Injuries and episodes ...")

panel <- panel %>%
  mutate(join_key = norm_name(PLAYER_NAME)) %>%
  left_join(clean_injuries %>%
              mutate(join_key = norm_name(PLAYER_NAME_NORM)) %>%
              arrange(SEASON, game_date, join_key, desc(IS_OUT), desc(IS_INJURY_ABSENCE)) %>%
              distinct(SEASON, game_date, join_key, .keep_all = TRUE) %>%
              select(SEASON, GAME_DATE = game_date, join_key,
                     INJURY_CATEGORY = injury_category, INJURY_REASON = reason,
                     IS_OUT, IS_QUESTIONABLE, IS_PROBABLE, IS_INJURY_ABSENCE, IS_STRUCTURAL),
            by = c("SEASON", "GAME_DATE", "join_key")) %>%
  mutate(
    INJURY_FLAG     = as.integer(!is.na(INJURY_CATEGORY)),
    INJURY_TIMELOSS = as.integer(PLAYED_FLAG == 0 & coalesce(IS_INJURY_ABSENCE, FALSE)),
    # IOC axes, parsed off the raw report text rather than collapsed into one label.
    INJURY_REGION   = injury_region_ioc(INJURY_REASON),
    INJURY_SIDE     = injury_side(INJURY_REASON),
    INJURY_TISSUE   = injury_tissue_ioc(INJURY_REASON),
    INJURY_MECH_RAW = injury_mechanism_ioc(INJURY_REASON),
    # Recurrences are matched on region AND side: a left hamstring after a right one is
    # a different injury, and SIC-2.0 treats side as part of the relationship.
    INJURY_SITE_KEY = if_else(INJURY_REGION == "unspecified", NA_character_,
                              paste(INJURY_REGION,
                                    if_else(INJURY_SIDE == "unspecified", "na", INJURY_SIDE),
                                    sep = "_"))
  ) %>%
  select(-join_key, -IS_OUT, -IS_INJURY_ABSENCE)

# ── 6. Injury episodes ── group consecutive absences, then split new vs reaggravation
panel <- panel %>%
  arrange(SEASON, PLAYER_ID, GAME_DATE) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(
    prev_played = dplyr::lag(PLAYED_FLAG, default = 1L),
    gp_running  = cumsum(PLAYED_FLAG),
    # A spell starts on the first missed game after a played one, or when the site
    # changes mid-absence (back then foot with no game between = a second injury).
    region_changed = as.integer(INJURY_TIMELOSS == 1 & prev_played == 0 &
                                  !is.na(INJURY_SITE_KEY) &
                                  !is.na(dplyr::lag(INJURY_SITE_KEY)) &
                                  INJURY_SITE_KEY != dplyr::lag(INJURY_SITE_KEY)),
    spell_start = as.integer((INJURY_TIMELOSS == 1 & prev_played == 1) | region_changed == 1),
    INJURY_EPISODE_ID = if_else(INJURY_TIMELOSS == 1, cumsum(spell_start), NA_integer_),
    TIMELOSS_GAMES = if_else(INJURY_TIMELOSS == 1,
                             ave(INJURY_TIMELOSS, cumsum(prev_played == 1), FUN = cumsum), 0L),
    # Site, tissue and mechanism from the FIRST report of an episode, carried across it,
    # so the whole spell can be read even where only one filing named a diagnosis.
    episode_region = zoo::na.locf(if_else(spell_start == 1, INJURY_SITE_KEY, NA_character_), na.rm = FALSE),
    episode_tissue = zoo::na.locf(if_else(spell_start == 1, INJURY_TISSUE, NA_character_), na.rm = FALSE),
    episode_mech   = zoo::na.locf(if_else(spell_start == 1 & INJURY_MECH_RAW != "unclassified",
                                          INJURY_MECH_RAW, NA_character_), na.rm = FALSE),
    ep_region = if_else(spell_start == 1 & !is.na(episode_region), episode_region, NA_character_),

    DAYS_SINCE_LAST_RETURN = if_else(
      INJURY_TIMELOSS == 1 & spell_start == 1,
      as.numeric(GAME_DATE - dplyr::lag(zoo::na.locf(if_else(PLAYED_FLAG == 1, GAME_DATE, NA_Date_),
                                                     na.rm = FALSE))),
      NA_real_),

    # Games played since the previous episode at the same site: within each site, the
    # gap in games-played between consecutive spell starts.
    GAMES_SINCE_LAST_SAME_SITE = {
      d <- tibble(site = ep_region, gp = gp_running, i = row_number()) %>%
        filter(!is.na(site)) %>%
        group_by(site) %>% mutate(v = gp - dplyr::lag(gp)) %>% ungroup()
      out <- rep(NA_real_, n()); out[d$i] <- d$v; out
    },

    # SIC-2.0 data-driven categories (Toohey et al. 2018): each episode described by its
    # relationship to the previous one on site and nature.
    SIC_CATEGORY = {
      d <- tibble(site = episode_region, tiss = episode_tissue, i = row_number(),
                  keep = spell_start == 1 & INJURY_TIMELOSS == 1) %>%
        filter(keep) %>%
        mutate(ss = !is.na(site) & !is.na(dplyr::lag(site)) & site == dplyr::lag(site),
               st = !is.na(tiss) & !is.na(dplyr::lag(tiss)) & tiss == dplyr::lag(tiss),
               v  = case_when(row_number() == 1 ~ "I index injury",
                              ss &  st ~ "II same site, same nature",
                              ss & !st ~ "VI same site, different nature",
                              !ss & st ~ "VII different site, same nature",
                              TRUE     ~ "VIII different site, different nature"))
      out <- rep(NA_character_, n()); out[d$i] <- d$v; out
    }
  ) %>%
  mutate(
    # A repeat at the same site is the old injury flaring up if the player managed fewer
    # than RETURN_DURABILITY_GAMES games in between; anything later is a new episode.
    IS_REAGGRAVATION = as.integer(spell_start == 1 & INJURY_TIMELOSS == 1 &
                                    !is.na(GAMES_SINCE_LAST_SAME_SITE) &
                                    GAMES_SINCE_LAST_SAME_SITE < RETURN_DURABILITY_GAMES),
    IS_NEW_INJURY = as.integer(spell_start == 1 & INJURY_TIMELOSS == 1 & IS_REAGGRAVATION == 0),
    INJURY_MECHANISM_IOC = coalesce(episode_mech, "unclassified"),
    INJURY_TISSUE_EP = episode_tissue,
    INJURY_SITE_EP   = episode_region,

    # Within-season history, all lagged so a row never sees its own episode.
    SEASON_GAMES_MISSED   = dplyr::lag(cumsum(PLAYED_FLAG == 0), default = 0L),
    PRIOR_EPISODES_SEASON = dplyr::lag(cumsum(IS_NEW_INJURY + IS_REAGGRAVATION), default = 0L),
    EVER_INJURED_SEASON   = as.integer(PRIOR_EPISODES_SEASON > 0),
    # Returned from an absence within the last fortnight: the acute re-injury window.
    RECENT_RETURN = as.integer(
      as.numeric(GAME_DATE - zoo::na.locf(
        if_else(PLAYED_FLAG == 1 & dplyr::lag(INJURY_TIMELOSS, default = 0L) == 1,
                GAME_DATE, NA_Date_), na.rm = FALSE)) <= RECENT_RETURN_DAYS)
  ) %>%
  ungroup() %>%
  mutate(RECENT_RETURN = replace_na(RECENT_RETURN, 0L)) %>%
  select(-prev_played, -region_changed, -spell_start, -gp_running,
         -episode_region, -episode_tissue, -episode_mech, -ep_region)

message("  episodes: ", sum(panel$IS_NEW_INJURY) + sum(panel$IS_REAGGRAVATION),
        " | new: ", sum(panel$IS_NEW_INJURY),
        " | reaggravation: ", sum(panel$IS_REAGGRAVATION))

# ── 7. Season structure ── 
allstar_dates <- tribble(
  ~SEASON,   ~ALLSTAR_DATE,
  "2021-22", as.Date("2022-02-20"),
  "2022-23", as.Date("2023-02-19"),
  "2023-24", as.Date("2024-02-18"),
  "2024-25", as.Date("2025-02-16"),
  "2025-26", as.Date("2026-02-15")
)

panel <- panel %>%
  left_join(allstar_dates, by = "SEASON") %>%
  group_by(SEASON) %>%
  mutate(DAYS_INTO_SEASON   = as.numeric(GAME_DATE - min(GAME_DATE)),
         PCT_THROUGH_SEASON = round(DAYS_INTO_SEASON / as.numeric(max(GAME_DATE) - min(GAME_DATE)), 4),
         IS_POST_ALLSTAR    = as.integer(GAME_DATE > ALLSTAR_DATE)) %>%
  ungroup() %>%
  # early = first ~6 weeks, mid = up to All-Star, late = the post-All-Star run-in
  mutate(season_phase = factor(case_when(DAYS_INTO_SEASON <= 42 ~ "early",
                                         IS_POST_ALLSTAR == 0   ~ "mid",
                                         TRUE                   ~ "late"),
                               levels = c("early", "mid", "late")))

# ── 8. Modelling panel ──
panel <- panel %>%
  select(
    SEASON, GAME_ID, GAME_DATE, PLAYER_ID, PLAYER_NAME, TEAM_ID, TEAM_TRICODE,
    TEAM_ABBREVIATION,
    IS_HOME, IS_AWAY, OPPONENT_TRICODE, weekNumber, monthNum, season_phase,
    DAYS_INTO_SEASON, PCT_THROUGH_SEASON, IS_POST_ALLSTAR,
    # Outcome and injury classification
    PLAYED_FLAG, INJURY_FLAG, INJURY_CATEGORY, INJURY_REASON,
    INJURY_REGION, INJURY_SIDE, INJURY_TISSUE, INJURY_SITE_KEY, INJURY_MECHANISM_IOC,
    INJURY_TISSUE_EP, INJURY_SITE_EP, SIC_CATEGORY, GAMES_SINCE_LAST_SAME_SITE,
    INJURY_TIMELOSS, INJURY_EPISODE_ID, TIMELOSS_GAMES, IS_NEW_INJURY, IS_REAGGRAVATION,
    IS_QUESTIONABLE, IS_PROBABLE, IS_STRUCTURAL,
    # Time axis and within-season history
    GAMES_PLAYED, PLAYER_DAYS_SINCE_LAST_PLAYED, SEASON_GAMES_MISSED,
    DAYS_SINCE_LAST_RETURN, PRIOR_EPISODES_SEASON, EVER_INJURED_SEASON, RECENT_RETURN,
    # Workload: minutes, on-court load, intensity
    ROLLING_MIN_7D, ROLLING_MIN_28D, ROLLING_MIN_42D, SEASON_CUMULATIVE_MIN, ACWR_MIN,
    ROLLING_GAMES_7D, ROLLING_GAMES_28D,
    ROLLING_LOAD_7D, ROLLING_LOAD_28D, ACWR_LOAD,
    INTENSITY, ROLLING_INTENSITY_7D, ROLLING_INTENSITY_28D,
    # Travel
    ROLLING_DIST_7D, ROLLING_DIST_28D, SEASON_CUMULATIVE_DIST, PLAYER_TRAVEL_KM,
    # Schedule stress: team, then player appearance
    TEAM_DAYS_REST, TEAM_IS_B2B, TEAM_TRAVEL_KM, TEAM_IS_LATE_GAME, TEAM_IS_ROAD_TRIP,
    TEAM_ROAD_TRIP_GAME_NUMBER, TEAM_ROAD_TRIP_LENGTH, TEAM_CUMULATIVE_ROAD_TRIP_KM,
    IS_B2B, IS_B2B_1ST, IS_B2B_2ND, SEASON_B2B_COUNT, IS_3IN4, IS_4IN5, IS_5IN7,
    ROLLING_B2B_7D, ROLLING_B2B_28D, IS_PLAYER_ROAD_TRIP, PLAYER_ROAD_TRIP_GAME_NUMBER,
    OPPONENT_DAYS_REST, OPPONENT_IS_B2B, OPPONENT_IS_ROAD_TRIP,
    # In-game: box score then tracking
    MIN, PTS, REB, AST, TOV, PLUS_MINUS,
    DISTANCE, SPEED, TOUCHES, USAGEPERCENTAGE, POSSESSIONS, PACE,
    # Player and career context
    PLAYER_AGE_AT_GAME, POSITION, HEIGHT_INCHES, WEIGHT, DRAFT_YEAR, DRAFT_NUMBER,
    CAREER_GAMES, CAREER_MINUTES, CAREER_GAMES_MISSED, SEASONS_IN_LEAGUE,
    PLAYED_PRESEASON, PRIOR_SEASON_GP, PRIOR_SEASON_MIN_PG, PRIOR_SEASON_PTS
  )

saveRDS(panel, file.path(BASE, "panel_2021-26.rds"))
message("\n=== Panel complete === rows: ", nrow(panel), " | cols: ", ncol(panel))

# ── Audit ── every injury enters through a name join, so a silent mismatch would drop
# real injuries and relabel those player-games as healthy. This is the check that
# justified building the skeleton from game logs rather than the roster snapshot.
reg_dates <- clean_sched %>% distinct(SEASON, GAME_DATE)
matched <- clean_injuries %>%
  filter(IS_INJURY_ABSENCE == 1, IS_OUT == 1) %>%
  semi_join(reg_dates, by = c("SEASON", "game_date" = "GAME_DATE")) %>%
  mutate(key = paste(SEASON, game_date, norm_name(PLAYER_NAME_NORM)))
panel_keys <- panel %>% transmute(key = paste(SEASON, GAME_DATE, norm_name(PLAYER_NAME)))
message("Injury-report -> panel match rate: ",
        round(100 * mean(matched$key %in% panel_keys$key), 1), "%  (",
        nrow(matched), " time-loss reports)")

# =============================================================================
# 5. Build Survival Dataset
# =============================================================================
# This is the last data step before modelling. I take my lean panel and reshape it
# into the counting-process format coxph() wants for time-varying covariates: one
# interval per player-game, a start/stop time, an event flag, and every covariate
# carried forward so no interval is missing its recent workload.
# Input:   data/processed/panel_2021-26.rds  (the lean panel from file 4)
# Output:  data/processed/cox_ready_2021-26.rds
#
# The counting-process format I'm building
# ----------------------------------------
#   One row per player per scheduled game, interval (t_start, t_stop]:
#     t_start = GAME_NUMBER - 1   t_stop = GAME_NUMBER   event = IS_NEW_INJURY
#   and the model I feed it to is:  Surv(t_start, t_stop, event) ~ covariates
#
# Who is "at risk" on each row (this is the bit I had to think hardest about)
# --------------------------------------------------------------------------
#   PLAYED_FLAG = 1                       -> at risk, covariates observed directly
#   PLAYED_FLAG = 0, INJURY_TIMELOSS = 0  -> at risk, covariates carried forward
#   PLAYED_FLAG = 0, INJURY_TIMELOSS = 1  -> NOT at risk (already injured), dropped
#   The one subtlety: I keep the FIRST game of an absence (the injury itself) in the
#   risk set via INJURY_TIMELOSS_LAGGED, otherwise I'd delete the very event I'm
#   trying to observe.
# =============================================================================

library(tidyverse)
library(lubridate)
library(survival)

BASE    <- here::here("data", "processed")   # repo-root/data/processed, portable
OUT_DIR <- BASE

message("=== Cox Model Preparation ===")

# ── 1. Load panel ──
panel <- readRDS(file.path(BASE, "panel_2021-26.rds"))
message("Panel loaded: ", nrow(panel), " rows x ", ncol(panel), " cols")

# A guard for myself — my panel should be regular-season only (game-ID prefix 002).
# File 3 already filters to 002, but I re-check here so that if a stray preseason /
# playoff / play-in / Cup game ever slips through, I catch it now rather than letting
# it quietly contaminate the model.
non_reg <- sum(str_sub(as.character(panel$GAME_ID), 1, 3) != "002", na.rm = TRUE)
if (non_reg > 0) {
  warning(non_reg, " non-regular-season game IDs found in the panel — these ",
          "should have been filtered upstream (file 3). Check the schedule filter.")
} else {
  message("  Regular-season check passed: all rows are 002 game IDs.")
}

# ── 2. Select modelling columns ──
# The lean panel is already trimmed down, but I re-select here so this one block is
# my single source of truth for exactly which columns (and in what order) go into
# the model. If I ever want to add or drop a covariate, this is the list I edit.
message("\n[1] Selecting modelling columns ...")

model_cols <- panel %>%
  select(
    
    # Identity (validation/grouping, not model inputs)
    SEASON, GAME_ID, GAME_DATE, PLAYER_ID, PLAYER_NAME,
    TEAM_ID, TEAM_ABBREVIATION,
    
    # Game context
    IS_HOME, IS_AWAY, OPPONENT_TRICODE,
    weekNumber, monthNum, season_phase,
    DAYS_INTO_SEASON, PCT_THROUGH_SEASON, IS_POST_ALLSTAR,
    
    # Participation & outcome
    PLAYED_FLAG, INJURY_TIMELOSS, IS_NEW_INJURY, IS_REAGGRAVATION,
    INJURY_EPISODE_ID, TIMELOSS_GAMES, INJURY_CATEGORY, INJURY_BODY_REGION,
    INJURY_MECHANISM,
    IS_QUESTIONABLE, IS_PROBABLE, IS_STRUCTURAL,
    
    # Time axis
    GAMES_PLAYED, PLAYER_DAYS_SINCE_LAST_PLAYED, SEASON_GAMES_MISSED,
    DAYS_SINCE_LAST_RETURN, DAYS_SINCE_LAST_SAME_REGION_RETURN,

    # Prior-injury history (time-varying, recurrent-events)
    PRIOR_EPISODES_SEASON, EVER_INJURED_SEASON, RECENT_RETURN,

    # Workload — minutes-based
    ROLLING_MIN_7D, ROLLING_MIN_14D, ROLLING_MIN_28D, ROLLING_MIN_42D,
    SEASON_CUMULATIVE_MIN, ACWR_MIN, ACWR_MIN_42, ACWR_SPIKE,
    ROLLING_GAMES_7D, ROLLING_GAMES_28D,
    # Workload — on-court external load (tracking distance)
    ROLLING_LOAD_7D, ROLLING_LOAD_28D, ACWR_LOAD,
    # Workload — on-court intensity (load per minute; decorrelated from minutes)
    INTENSITY, ROLLING_INTENSITY_7D, ROLLING_INTENSITY_28D,

    # Travel
    ROLLING_DIST_7D, ROLLING_DIST_14D, ROLLING_DIST_28D, ROLLING_DIST_42D,
    SEASON_CUMULATIVE_DIST, PLAYER_TRAVEL_KM,
    
    # Schedule stress — team
    TEAM_DAYS_REST, TEAM_IS_B2B, TEAM_TRAVEL_KM, TEAM_IS_LATE_GAME,
    TEAM_IS_ROAD_TRIP, TEAM_ROAD_TRIP_GAME_NUMBER,
    TEAM_ROAD_TRIP_LENGTH, TEAM_CUMULATIVE_ROAD_TRIP_KM,
    
    # Schedule stress — player appearance
    IS_B2B, IS_B2B_1ST, IS_B2B_2ND, SEASON_B2B_COUNT,
    IS_3IN4, IS_4IN5, IS_5IN7,
    ROLLING_B2B_7D, ROLLING_B2B_28D,
    IS_PLAYER_ROAD_TRIP, PLAYER_ROAD_TRIP_GAME_NUMBER,
    
    # Opponent context
    OPPONENT_DAYS_REST, OPPONENT_IS_B2B, OPPONENT_IS_ROAD_TRIP,
    
    # In-game load — box score
    MIN, PTS, REB, AST, TOV, PLUS_MINUS,
    
    # In-game load — tracking
    DISTANCE, SPEED, TOUCHES, USAGEPERCENTAGE, POSSESSIONS, PACE,
    
    # Player physical context
    PLAYER_AGE_AT_GAME, POSITION, HEIGHT_INCHES, WEIGHT, DRAFT_YEAR, DRAFT_NUMBER,
    
    # Career load context
    CAREER_GAMES, CAREER_MINUTES, CAREER_GAMES_MISSED, SEASONS_IN_LEAGUE,
    PLAYED_PRESEASON,
    PRIOR_SEASON_GP, PRIOR_SEASON_MIN_PG, PRIOR_SEASON_PTS,
  )

message("  Modelling columns selected: ", ncol(model_cols))

# ── 3. LOCF (last observation carried forward) for missed-game rows ──
# On a missed game the player didn't appear, so all their in-game covariates are NA.
# I carry the last observed value forward so every interval still has a complete set
# of covariates. This is the right thing conceptually too: the load a player was
# carrying going INTO an absence is exactly the exposure I want attached to it.
message("\n[2] Carrying forward covariates for missed-game rows (LOCF) ...")

locf_cols <- c(
  "ROLLING_MIN_7D", "ROLLING_MIN_14D", "ROLLING_MIN_28D", "ROLLING_MIN_42D",
  "ROLLING_DIST_7D", "ROLLING_DIST_14D", "ROLLING_DIST_28D", "ROLLING_DIST_42D",
  "SEASON_CUMULATIVE_MIN", "SEASON_CUMULATIVE_DIST",
  "ROLLING_GAMES_7D", "ROLLING_GAMES_28D",
  "ROLLING_LOAD_7D", "ROLLING_LOAD_28D", "ACWR_LOAD",   # on-court external load
  "INTENSITY", "ROLLING_INTENSITY_7D", "ROLLING_INTENSITY_28D",   # on-court load per minute
  "ROLLING_B2B_7D", "ROLLING_B2B_28D",
  "ACWR_MIN", "ACWR_MIN_42", "ACWR_SPIKE",
  "IS_B2B", "IS_B2B_1ST", "IS_B2B_2ND", "SEASON_B2B_COUNT",
  "IS_3IN4", "IS_4IN5", "IS_5IN7",
  "IS_PLAYER_ROAD_TRIP", "PLAYER_ROAD_TRIP_GAME_NUMBER",
  "GAMES_PLAYED", "PLAYER_DAYS_SINCE_LAST_PLAYED",
  "DISTANCE", "SPEED", "TOUCHES", "USAGEPERCENTAGE", "POSSESSIONS", "PACE",
  "MIN", "PTS", "REB", "AST", "TOV", "PLUS_MINUS"
)

model_cols <- model_cols %>%
  arrange(SEASON, PLAYER_ID, GAME_DATE) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(across(
    all_of(intersect(locf_cols, names(.))),
    ~ zoo::na.locf(.x, na.rm = FALSE)
  )) %>%
  ungroup()

# ── 3b. Prospective (previous-played-game) versions of the in-game covariates ──
# The in-game covariates (minutes, distance, intensity, box score) are realised
# DURING a game, so using the CURRENT game's value on an interval means conditioning
# on within-interval information, and it is timed inconsistently: a HEALTHY player at
# a given risk time contributes their current game, but an INJURED player's row is
# LOCF from their last played game (one game earlier). To make these covariates proper
# (known before the interval starts) and consistently timed across healthy vs injured,
# I add, for EVERY row, the value of the game the player most recently PLAYED strictly
# BEFORE this one: PREV_GAME_* (the previous played game) and PREV_2GAME_* (two played
# games back). I keep the current-game columns as well; which version the model uses is
# decided in "6. Cox Modelling", not here.
ingame_cols <- intersect(
  c("MIN", "DISTANCE", "INTENSITY", "SPEED", "TOUCHES", "USAGEPERCENTAGE",
    "POSSESSIONS", "PACE", "PTS", "REB", "AST", "TOV", "PLUS_MINUS"),
  names(model_cols)
)

# The k-th most recent PLAYED value strictly before each row (k = 1 -> previous game,
# k = 2 -> two games back). `before` = how many games the player has PLAYED before this
# row, so I index straight into the ordered sequence of their played values.
lag_played <- function(x, played, before, k) {
  seq_vals <- x[played == 1L]
  idx <- before - (k - 1L)
  as.numeric(ifelse(idx >= 1L, seq_vals[pmax(idx, 1L)], NA_real_))
}

model_cols <- model_cols %>%
  arrange(SEASON, PLAYER_ID, GAME_DATE) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(.played_before = lag(cumsum(PLAYED_FLAG), default = 0L)) %>%
  mutate(across(
    all_of(ingame_cols),
    list(PREV_GAME  = ~ lag_played(.x, PLAYED_FLAG, .played_before, 1L),
         PREV_2GAME = ~ lag_played(.x, PLAYED_FLAG, .played_before, 2L)),
    .names = "{.fn}_{.col}"
  )) %>%
  select(-.played_before) %>%
  ungroup()

# ── 4. Build the counting-process intervals ──
# My time axis, GAME_NUMBER, counts EVERY row (played + missed), so "time" is games,
# not calendar days. I then drop the injured rows from the risk set — but I use
# INJURY_TIMELOSS_LAGGED to keep the first game of an absence (the event itself) in,
# because that's the row I need to observe the injury on.
message("\n[3] Building counting process intervals ...")

cox_data <- model_cols %>%
  arrange(SEASON, PLAYER_ID, GAME_DATE) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(
    GAME_NUMBER = row_number(),
    t_start = GAME_NUMBER - 1L,
    t_stop  = GAME_NUMBER,
    # My primary event: the first missed game of a NEW injury episode.
    event = as.integer(IS_NEW_INJURY == 1),
    # My recurrent-events outcome (for the Andersen-Gill companion model): ANY episode
    # start, new OR reaggravation. If I left the reaggravation rows as non-events, I'd
    # be understating how often players actually break down.
    event_recurrent = as.integer(IS_NEW_INJURY == 1 | IS_REAGGRAVATION == 1),
    # My mechanism-specific outcomes — the negative-control test. I split new injuries
    # by how much accumulated load I'd expect to drive them:
    #   event_soft   = new SOFT-TISSUE injury (overuse) — load SHOULD drive this
    #   event_load   = new load-linked STRUCTURAL injury (ACL, ankle sprain, stress #)
    #   event_impact = new RANDOM-IMPACT injury (finger, eye, nose) — my CLEAN control
    # For any one of these, an injury of a DIFFERENT type is just a 0 and the player
    # leaves the risk set when they go Out — which is exactly the cause-specific hazard
    # handling I want.
    event_soft   = as.integer(IS_NEW_INJURY == 1 & INJURY_MECHANISM == "soft_tissue"),
    event_load   = as.integer(IS_NEW_INJURY == 1 & INJURY_MECHANISM == "traumatic_load"),
    event_impact = as.integer(IS_NEW_INJURY == 1 & INJURY_MECHANISM == "random_impact"),
    event_soft_load   = as.integer(IS_NEW_INJURY == 1 & (INJURY_MECHANISM == "traumatic_load" | INJURY_MECHANISM == "soft_tissue")),
    event_soft_recurrent   = as.integer((IS_NEW_INJURY == 1 | IS_REAGGRAVATION == 1) & INJURY_MECHANISM == "soft_tissue"),
    event_load_recurrent   = as.integer((IS_NEW_INJURY == 1 | IS_REAGGRAVATION == 1) & INJURY_MECHANISM == "traumatic_load"),
    event_impact_recurrent = as.integer((IS_NEW_INJURY == 1 | IS_REAGGRAVATION == 1) & INJURY_MECHANISM == "random_impact"),
    event_soft_load_recurrent = as.integer((IS_NEW_INJURY == 1 | IS_REAGGRAVATION == 1) & (INJURY_MECHANISM == "traumatic_load" | INJURY_MECHANISM == "soft_tissue")),

    # Who is in the risk set. I only want to remove rows where the player was ALREADY
    # out when the interval opened AND is still out — i.e. the middle of an absence.
    # Spelling it out for all four cases:
    #   played / previous not absent  -> IN  (the normal row)
    #   onset  / previous not absent  -> IN  (the event itself; this is why the lag exists)
    #   absent / previous absent      -> OUT (mid-absence, genuinely not at risk)
    #   played / previous absent      -> IN  (FIRST GAME BACK)
    # That last case is the fix. The earlier rule was "previous row wasn't timeloss",
    # which dropped the first game back as well - 4,690 played games, 3.6% of all
    # appearances, and exactly the return-to-play games RECENT_RETURN is meant to
    # measure. It removed exposure rather than events, but it removed the wrong ones.
    INJURY_TIMELOSS_LAGGED = lag(INJURY_TIMELOSS, default = 0L),
    IN_RISK_SET = as.integer(!(INJURY_TIMELOSS == 1 & INJURY_TIMELOSS_LAGGED == 1)),

    # Censored at the last in-risk-set row of the player-season if no event
    IS_CENSORED = as.integer(GAME_NUMBER == max(GAME_NUMBER[IN_RISK_SET == 1]) & event == 0)
  ) %>%
  ungroup()

# A QC check I want visible, not silent: how many events survive the risk-set filter.
# The awkward case is a region-change spell that STARTS mid-absence (the previous row
# is still timeloss) — it has IN_RISK_SET = 0, so it gets dropped with the injured
# rows and takes its event with it. I count that loss for both outcomes so I can see
# it and defend it rather than pretend it isn't happening.
qc_pre  <- cox_data %>% summarise(new = sum(event), rec = sum(event_recurrent))
qc_post <- cox_data %>% filter(IN_RISK_SET == 1) %>%
  summarise(new = sum(event), rec = sum(event_recurrent))
message("  Event retention through risk-set filter (mid-absence region changes):")
message("    New-injury events: ", qc_pre$new, " -> ", qc_post$new,
        "  (", qc_pre$new - qc_post$new, " dropped)")
message("    Recurrent events:  ", qc_pre$rec, " -> ", qc_post$rec,
        "  (", qc_pre$rec - qc_post$rec, " dropped)")

cox_data <- cox_data %>%
  filter(IN_RISK_SET == 1) %>% select(-any_of(c("INJURY_TIMELOSS_LAGGED")))

cox_data <- cox_data %>%
  # I drop players who never appeared at all in a season — with no observed workload
  # they were never genuinely at risk in the way my model measures.
  group_by(SEASON, PLAYER_ID) %>%
  filter(any(PLAYED_FLAG == 1)) %>%
  ungroup() %>%

  # I also drop the rows BEFORE a player's first appearance (this is left-truncation).
  # GAMES_PLAYED is only NA in that pre-appearance window — a season that opens with
  # the player already out. Those rows have no observed workload, so they can't inform
  # the model, and na.omit would drop them anyway. The catch is that if one of them is
  # also an injury event, na.omit would silently lose the event — so I drop them here
  # on purpose instead. My model measures WITHIN-season risk from observed load; a
  # player just enters the risk set when they start playing.
  filter(!is.na(GAMES_PLAYED)) %>%

  # Rookies need care — I set their career values to real zeros (not imputed guesses)
  # and flag them so the model can treat them as their own case.
  mutate(
    IS_ROOKIE = as.integer(SEASONS_IN_LEAGUE == 1 | is.na(SEASONS_IN_LEAGUE)),
    
    CAREER_MINUTES    = case_when(IS_ROOKIE == 1 ~ 0,  TRUE ~ CAREER_MINUTES),
    CAREER_GAMES      = case_when(IS_ROOKIE == 1 ~ 0L, TRUE ~ CAREER_GAMES),
    CAREER_GAMES_MISSED = case_when(IS_ROOKIE == 1 ~ 0L, TRUE ~ CAREER_GAMES_MISSED),
    SEASONS_IN_LEAGUE = case_when(IS_ROOKIE == 1 ~ 1L, TRUE ~ SEASONS_IN_LEAGUE),
    
    # Rookies genuinely have no prior season, so zero is the correct value for them.
    # For a NON-rookie a missing prior season means something different: the record is
    # absent (a season-long absence, G League, or overseas), NOT that they played zero
    # minutes. Zero-filling those would mislabel returning players as deep bench and
    # quietly drop them from any prior-season cohort filter, so they stay NA.
    PRIOR_SEASON_GP     = if_else(IS_ROOKIE == 1, replace_na(PRIOR_SEASON_GP, 0),
                                  PRIOR_SEASON_GP),
    PRIOR_SEASON_MIN_PG = if_else(IS_ROOKIE == 1, replace_na(PRIOR_SEASON_MIN_PG, 0),
                                  PRIOR_SEASON_MIN_PG),
  ) %>%
  
  # I cast the binary flags to a clean 0/1 integer. Two kinds of NaN I fill with 0,
  # because here a NaN genuinely means "this didn't happen":
  #   (a) injury-report flags — no report row means the player simply wasn't flagged
  #   (b) appearance flags on a player's FIRST game — there's no prior game, so a
  #       B2B / 3-in-4 is impossible by definition (= 0). This one matters: a first
  #       game can be an EVENT row, and if I left it NaN coxph() would drop the event.
  # I run as.numeric() first so both the logical (TRUE/FALSE) and float (0.0/1.0)
  # forms get normalised before I fill NA->0 and cast to integer.
  # One thing I'm careful NOT to fill: TEAM_IS_LATE_GAME / TEAM_TRAVEL_KM. Their NaNs
  # come from an upstream arena-lookup gap, not a "didn't occur", so zero-filling them
  # would be a lie. Same for ACWR and the other continuous covariates — their NaNs are
  # real burn-in, and I let na.action = na.omit handle those instead.
  mutate(
    across(
      c(IS_QUESTIONABLE, IS_PROBABLE, IS_STRUCTURAL,
        IS_B2B, IS_B2B_1ST, IS_B2B_2ND,
        IS_3IN4, IS_4IN5, IS_5IN7,
        IS_PLAYER_ROAD_TRIP, PLAYER_ROAD_TRIP_GAME_NUMBER,
        SEASON_B2B_COUNT),
      ~ as.integer(replace_na(as.numeric(.x), 0))
    )
  ) %>%
  
  # Cleaning up PACE. PACE is possessions per 48 minutes, so physically it lives
  # around 80-120 (the league sits ~95-105). The problem is garbage-time cameos: a
  # player on court for a few seconds divides ~1 possession by near-zero minutes and
  # gets an absurd PACE (I saw 28,800 from six seconds of play). The row is real but
  # the rate is meaningless, so I null PACE outside [70, 130] and let na.omit deal
  # with those rows. It's only ~0.9% of rows, spread evenly across seasons.
  mutate(
    PACE = if_else(PACE < 70 | PACE > 130, NA_real_, PACE)
  )
# I leave the ACWR burn-in rows in the risk set on purpose — coxph()'s
# na.action = na.omit just leaves them out of the partial likelihood for me.

message("  Counting process rows (in risk set): ", nrow(cox_data))
message("  Events — new: ", sum(cox_data$event),
        " | recurrent: ", sum(cox_data$event_recurrent),
        " | soft-tissue: ", sum(cox_data$event_soft))
message("  Unique players: ", n_distinct(cox_data$PLAYER_ID))

# Interval sanity — the counting-process intervals must be well-formed.
stopifnot(all(cox_data$t_stop > cox_data$t_start),
          !any(is.na(cox_data$t_start) | is.na(cox_data$t_stop) | is.na(cox_data$event)))

# ── 5. Save ──
saveRDS(cox_data, file.path(OUT_DIR, "cox_ready_2021-26.rds"))
message("\n=== cox_ready saved === rows: ", nrow(cox_data), " | cols: ", ncol(cox_data))



# 5. Build Survival Dataset
# Reshapes the panel into the counting-process format coxph() wants for time-varying
# covariates: one interval per player-game, (t_start, t_stop], and an event flag. The
# rolling workload windows arrive from script 4 already defined on every row and timed
# to end the day before the game; per-game values are carried forward and then lagged.
# Input:   data/processed/panel_2021-26.rds
# Output:  data/processed/cox_ready_2021-26.rds
#
# Who is at risk on each row:
#   played                          -> at risk, covariates observed
#   absent but not injured (rested) -> at risk, per-game covariates carried forward
#   already serving an injury spell -> NOT at risk, dropped

library(tidyverse)
library(survival)

BASE <- here::here("data", "processed")

message("=== Cox Model Preparation ===")

panel <- readRDS(file.path(BASE, "panel_2021-26.rds"))
stopifnot(all(str_sub(as.character(panel$GAME_ID), 1, 3) == "002"))
message("Panel loaded: ", nrow(panel), " rows x ", ncol(panel), " cols")

# ── 1. Carry per-game covariates forward onto missed games ──
# On a missed game the player did not appear, so their in-game covariates are NA; the
# last observed value is carried forward, and section 2 then lags it so a row only ever
# sees games played before it. The rolling windows (ROLLING_*, SEASON_CUMULATIVE_*,
# ACWR_*) are NOT carried: script 4 builds them on every row from appearances dated
# before the game, so a rested player's window decays as it should and an onset row
# is timed the same way as every other row in its risk set.
message("\n[1] Carrying per-game covariates forward (LOCF) ...")

locf_cols <- c(
  "INTENSITY",
  "IS_B2B", "IS_B2B_1ST", "IS_B2B_2ND", "IS_3IN4", "IS_4IN5", "IS_5IN7",
  "IS_PLAYER_ROAD_TRIP", "PLAYER_ROAD_TRIP_GAME_NUMBER",
  "GAMES_PLAYED", "PLAYER_DAYS_SINCE_LAST_PLAYED",
  "MIN", "PTS", "REB", "AST", "TOV", "PLUS_MINUS",
  "DISTANCE", "SPEED", "TOUCHES", "USAGEPERCENTAGE", "POSSESSIONS", "PACE"
)

cox_data <- panel %>%
  arrange(SEASON, PLAYER_ID, GAME_DATE) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(across(all_of(locf_cols), ~ zoo::na.locf(.x, na.rm = FALSE)))

# ── 2. Prospective versions of the in-game covariates ──
# In-game covariates are realised DURING a game, so using the current game's value
# conditions on within-interval information, and it is timed inconsistently: a healthy
# player contributes their current game while an injured player's row is carried
# forward from the last one they played. Instead every row gets the value of the game
# the player most recently PLAYED before this one (lag 1) and the one before that
# (lag 2), both of which are known before the interval opens.
message("[2] Building prospective lags ...")

ingame_cols <- c("MIN", "DISTANCE", "INTENSITY", "SPEED", "TOUCHES", "USAGEPERCENTAGE",
                 "POSSESSIONS", "PACE", "PTS", "REB", "AST", "TOV", "PLUS_MINUS")

# `Counts how many games the player has played prior to this row, so it indexes
# straight into the ordered sequence of the values they actually recorded.
lag_played <- function(x, played, before, k) {
  vals <- x[played == 1L]
  idx  <- before - (k - 1L)
  as.numeric(ifelse(idx >= 1L, vals[pmax(idx, 1L)], NA_real_))
}

cox_data <- cox_data %>%
  mutate(.played_before = dplyr::lag(cumsum(PLAYED_FLAG), default = 0L)) %>%
  mutate(across(all_of(ingame_cols),
                list(PREV_GAME  = ~ lag_played(.x, PLAYED_FLAG, .played_before, 1L),
                     PREV_2GAME = ~ lag_played(.x, PLAYED_FLAG, .played_before, 2L)),
                .names = "{.fn}_{.col}")) %>%
  select(-.played_before)

# ── 3. Counting-process intervals ──
# The clock counts every scheduled game, played or missed, so time is the player's
# position through their team's season rather than games accumulated.
message("[3] Building intervals ...")

cox_data <- cox_data %>%
  mutate(
    GAME_NUMBER = row_number(),
    t_start = GAME_NUMBER - 1L,
    t_stop  = GAME_NUMBER,
    # Primary outcome: the first missed game of a new episode. The recurrent outcome
    # adds reaggravations, since leaving them as non-events would understate how often
    # players actually break down.
    event           = as.integer(IS_NEW_INJURY == 1),
    event_recurrent = as.integer(IS_NEW_INJURY == 1 | IS_REAGGRAVATION == 1),
    # Mechanism-specific outcomes on the IOC axis: the negative-control split.
    # repetitive = overuse, where a workload effect belongs; acute_nc = a discrete event
    # fatigue can set up; acute_imp = external energy transfer, which load cannot cause.
    event_repetitive = as.integer(event == 1 & INJURY_MECHANISM_IOC == "repetitive"),
    event_acute_nc   = as.integer(event == 1 & INJURY_MECHANISM_IOC == "acute_noncontact"),
    event_acute_imp  = as.integer(event == 1 & INJURY_MECHANISM_IOC == "acute_impact"),
    event_repetitive_recurrent = as.integer(event_recurrent == 1 & INJURY_MECHANISM_IOC == "repetitive"),
    event_acute_nc_recurrent   = as.integer(event_recurrent == 1 & INJURY_MECHANISM_IOC == "acute_noncontact"),
    event_acute_imp_recurrent  = as.integer(event_recurrent == 1 & INJURY_MECHANISM_IOC == "acute_impact"),
    # Out only if already absent when the interval opened AND still absent: that keeps
    # the onset row and the first game back in the risk set.
    IN_RISK_SET = as.integer(!(INJURY_TIMELOSS == 1 & dplyr::lag(INJURY_TIMELOSS, default = 0L) == 1)),
    IS_CENSORED = as.integer(GAME_NUMBER == max(GAME_NUMBER[IN_RISK_SET == 1]) & event == 0)
  ) %>%
  ungroup()

# A new injury to a different region that starts mid-absence is dropped with the
# absence around it, so the loss is counted rather than left silent.
message("  Events through the risk-set filter: new ", sum(cox_data$event), " -> ",
        sum(cox_data$event[cox_data$IN_RISK_SET == 1]), " | recurrent ",
        sum(cox_data$event_recurrent), " -> ",
        sum(cox_data$event_recurrent[cox_data$IN_RISK_SET == 1]))

# ── 4. Risk set and covariate tidy-up ──
cox_data <- cox_data %>%
  filter(IN_RISK_SET == 1) %>%
  # Players who never appeared were never genuinely at risk in the way this model
  # measures, and rows before a player's first appearance are left-truncated.
  # GAMES_PLAYED is NA only in that pre-appearance window; dropping those rows here
  # rather than letting na.omit do it means an event inside one is not lost silently.
  group_by(SEASON, PLAYER_ID) %>%
  filter(any(PLAYED_FLAG == 1)) %>%
  ungroup() %>%
  filter(!is.na(GAMES_PLAYED)) %>%
  mutate(
    # Rookies get real zeros for career history, flagged so the model can treat them
    # as their own case. A missing prior season for a NON-rookie means the record is
    # absent, not that they played zero, those stay NA
    IS_ROOKIE = as.integer(SEASONS_IN_LEAGUE == 1 | is.na(SEASONS_IN_LEAGUE)),
    across(c(CAREER_MINUTES, CAREER_GAMES, CAREER_GAMES_MISSED),
           ~ if_else(IS_ROOKIE == 1, 0, as.numeric(.x))),
    SEASONS_IN_LEAGUE   = if_else(IS_ROOKIE == 1, 1L, SEASONS_IN_LEAGUE),
    PRIOR_SEASON_GP     = if_else(IS_ROOKIE == 1, replace_na(PRIOR_SEASON_GP, 0), PRIOR_SEASON_GP),
    PRIOR_SEASON_MIN_PG = if_else(IS_ROOKIE == 1, replace_na(PRIOR_SEASON_MIN_PG, 0), PRIOR_SEASON_MIN_PG),
    across(c(IS_QUESTIONABLE, IS_PROBABLE, IS_STRUCTURAL, IS_B2B, IS_B2B_1ST, IS_B2B_2ND,
             IS_3IN4, IS_4IN5, IS_5IN7, IS_PLAYER_ROAD_TRIP, PLAYER_ROAD_TRIP_GAME_NUMBER,
             SEASON_B2B_COUNT),
           ~ as.integer(replace_na(as.numeric(.x), 0))),
    # PACE is possessions per 48 minutes, so it lives around 80-120. A few seconds of
    # garbage time divides one possession by near-zero minutes and returns nonsense.
    PACE = if_else(between(PACE, 70, 130), PACE, NA_real_)
  )

stopifnot(all(cox_data$t_stop > cox_data$t_start),
          !any(is.na(cox_data$t_start) | is.na(cox_data$t_stop) | is.na(cox_data$event)))

saveRDS(cox_data, file.path(BASE, "cox_ready_2021-26.rds"))

message("\n=== cox_ready saved === rows: ", nrow(cox_data), " | cols: ", ncol(cox_data))
message("  events - new: ", sum(cox_data$event),
        " | recurrent: ", sum(cox_data$event_recurrent),
        " | repetitive: ", sum(cox_data$event_repetitive),
        " | players: ", n_distinct(cox_data$PLAYER_ID))

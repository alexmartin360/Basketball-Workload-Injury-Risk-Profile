# =============================================================================
# 4. Assemble Player-Game Panel
# =============================================================================
# This is the heart of my data build. I take all the cleaned pieces and stitch
# them into one big table with a row for every player in every one of their
# team's games — played or missed. Onto that skeleton I derive everything my model
# needs: the rolling workload and travel windows, the schedule-density flags, the
# injury episodes (splitting a brand-new injury from a reaggravation of an old one)
# and the season-structure / training-camp context.
#
# I save two versions: a fat one with every column I built (handy for EDA) and a
# lean one with just the modelling columns, which is what file 5 picks up.
# Sources: 1. Injury Classification Rules.R
# Input:   data/processed/clean_{sched,game_logs,rosters,player_bio,career,
#          game_load,injuries}.rds, preseason_participation.rds
# Output:  data/processed/full_panel_2021-26.rds  (all columns, for EDA)
#          data/processed/panel_2021-26.rds        (lean, for file 5)
# =============================================================================

library(tidyverse)
library(lubridate)
library(slider)

SEASONS <- c("2021-22", "2022-23", "2023-24", "2024-25", "2025-26")

BASE <- here::here("data", "processed")   # repo-root/data/processed, portable

# Dictionary functions (injury_body_region etc.) + make this script self-contained
source(here::here("analysis", "1. Injury Classification Rules.R"))

clean_sched      <- read_rds(file.path(BASE, "clean_sched.rds"))
clean_game_logs  <- read_rds(file.path(BASE, "clean_game_logs.rds"))
clean_rosters    <- read_rds(file.path(BASE, "clean_rosters.rds"))
clean_player_bio <- read_rds(file.path(BASE, "clean_player_bio.rds"))
clean_career     <- read_rds(file.path(BASE, "clean_career.rds"))
clean_injuries   <- read_rds(file.path(BASE, "clean_injuries.rds"))
clean_game_load  <- read_rds(file.path(BASE, "clean_game_load.rds"))
preseason_participation <- read_rds(file.path(BASE, "preseason_participation.rds"))

message("\n--- SECTION 3: Join ---")

# ── 3a. Panel skeleton ── player tenure x schedule, then left-join the game logs
# so I can mark each game played or missed.
# I rebuilt this early on and it mattered a lot. My player universe is now every
# player who ACTUALLY APPEARED in a regular-season game (taken from the game logs),
# not the CommonTeamRoster snapshot I started with. That snapshot quietly dropped
# ~32% of players who played (the waived / traded / 10-day / two-way guys) and with
# them ~8.5% of my genuine-injury rows — which is a health-selection bias sitting
# right on the outcome I'm trying to model. I check this in the audit at the bottom.
#
# I only give a player rows inside their TEAM-TENURE window, worked out per player-
# team stint from the game logs, so I neither miss the transient players nor invent
# "missed" games outside their actual time on a team. The window is their full
# PRESENCE span [win_start, win_end], where presence = played a regular-season game
# OR filed Out on the injury report — that way a season-OPENING or season-ENDING
# injury absence (which sits outside the played-games span) still shows up as an
# at-risk row.

# My name normaliser. Names are the weak link between the injury reports and the
# game logs, so I hammer both sides into the same shape: accents down to plain
# ASCII, punctuation dropped, and the "Jr/Sr/III" suffixes stripped off. I reuse
# it (a) to map injury names to PLAYER_ID for the tenure step here, and (b) for the
# actual injury join in 3f.
norm_name <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- str_replace_all(x, "[.']", "")
  x <- str_replace_all(x, "-", " ")
  x <- str_replace_all(x, "\\b(jr|sr|ii|iii|iv|v)\\b", "")
  str_squish(x)
}

# Regular-season game logs only (002 prefix) — preseason logs must not seed tenure.
gl_reg <- clean_game_logs %>%
  filter(str_sub(as.character(GAME_ID), 1, 3) == "002")

# I map each injury Out date to a PLAYER_ID using the normalised-name lookup. I keep
# the INDIVIDUAL dates (not just a min/max span) so I can attribute each one to the
# right team stint below. This matters for traded players: if an injury falls in the
# GAP between the last game for one team and the first game for the next (say a
# mid-season MCL injury while a trade is going through), a min/max span would pin it
# on the wrong team.
id_lookup <- gl_reg %>% distinct(SEASON, PLAYER_ID, PLAYER_NAME) %>%
  mutate(nkey = norm_name(PLAYER_NAME))
inj_dates <- clean_injuries %>%
  filter(IS_INJURY_ABSENCE == 1, IS_OUT == 1) %>%
  mutate(nkey = norm_name(PLAYER_NAME_NORM)) %>%
  inner_join(id_lookup, by = c("SEASON", "nkey"), relationship = "many-to-many") %>%
  distinct(SEASON, PLAYER_ID, inj_date = game_date)

# I build each player-team stint (bounded by the games they played), rank them
# within the player-season, and note when the NEXT stint starts so each stint knows
# it owns the stretch of time up to the next one.
stints0 <- gl_reg %>%
  group_by(SEASON, PLAYER_ID, TEAM_ID) %>%
  summarise(
    PLAYER_NAME       = first(PLAYER_NAME),
    TEAM_ABBREVIATION = first(TEAM_ABBREVIATION),
    first_played      = min(GAME_DATE),
    last_played       = max(GAME_DATE),
    .groups = "drop"
  ) %>%
  arrange(SEASON, PLAYER_ID, first_played) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(stint_rank = row_number(), n_stints = n(), next_first = lead(first_played)) %>%
  ungroup()

# Now I attribute each injury Out date to the stint whose [lo, hi) interval contains
# it. The rules I settled on: the FIRST stint also owns any dates before its first
# game (a season-opening injury); the LAST stint owns dates after its first game
# (season-ending stuff and trailing absences); and a MIDDLE stint owns up to the
# next stint's start, so a traded player's between-teams injury lands on the team he
# was actually on. Only injury-covered dates stretch a window, so a genuinely
# unrostered gap (waived, unsigned) is left empty rather than fabricated.
inj_by_stint <- stints0 %>%
  select(SEASON, PLAYER_ID, TEAM_ID, stint_rank, n_stints, first_played, next_first) %>%
  left_join(inj_dates, by = c("SEASON", "PLAYER_ID"), relationship = "many-to-many") %>%
  mutate(
    lo = if_else(stint_rank == 1L,       as.Date("1900-01-01"), first_played),
    hi = if_else(stint_rank == n_stints, as.Date("2100-01-01"), next_first)
  ) %>%
  filter(!is.na(inj_date), inj_date >= lo, inj_date < hi) %>%
  group_by(SEASON, PLAYER_ID, TEAM_ID) %>%
  summarise(inj_lo = min(inj_date), inj_hi = max(inj_date), .groups = "drop")

# Each stint's tenure window is the span of games they played, stretched out to
# cover any injury dates that stint owns.
stints <- stints0 %>%
  left_join(inj_by_stint, by = c("SEASON", "PLAYER_ID", "TEAM_ID")) %>%
  mutate(
    win_start = pmin(first_played, coalesce(inj_lo, first_played)),
    win_end   = pmax(last_played,  coalesce(inj_hi, last_played))
  )

# Here's the skeleton itself: I cross each stint with its team's regular-season
# games that fall inside the tenure window, then left-join the player's box score.
# If the box score is there they played; if it's missing (NA minutes) they missed.
panel <- stints %>%
  select(SEASON, PLAYER_ID, PLAYER_NAME, TEAM_ID, TEAM_ABBREVIATION, win_start, win_end) %>%
  inner_join(
    clean_sched %>% select(SEASON, GAME_ID, GAME_DATE, TEAM_ID, everything()),
    by = c("SEASON", "TEAM_ID"),
    relationship = "many-to-many"
  ) %>%
  filter(GAME_DATE >= win_start, GAME_DATE <= win_end) %>%
  select(-win_start, -win_end) %>%
  left_join(
    gl_reg %>% select(
      SEASON, PLAYER_ID, GAME_ID, TEAM_ID,
      MIN, PTS, REB, AST, STL, BLK, TOV, PF, PLUS_MINUS,
      FGM, FGA, FG_PCT, FG3M, FG3A, FG3_PCT,
      FTM, FTA, FT_PCT, OREB, DREB, BLKA, PFD, WL
    ),
    by = c("SEASON", "PLAYER_ID", "GAME_ID", "TEAM_ID")
  ) %>%
  mutate(PLAYED_FLAG = as.integer(!is.na(MIN))) %>%
  # The one rule my whole counting-process setup depends on: exactly one row per
  # player per calendar date. A few overlaps sneak in when a player is waived and
  # re-signed by the same team (their stint windows touch), so where there's a tie
  # I keep the played row.
  arrange(SEASON, PLAYER_ID, GAME_DATE, desc(PLAYED_FLAG)) %>%
  distinct(SEASON, PLAYER_ID, GAME_DATE, .keep_all = TRUE)

# QC: confirm the one-row-per-player-per-date invariant now holds.
dup_dates <- panel %>% count(SEASON, PLAYER_ID, GAME_DATE) %>% filter(n > 1)
if (nrow(dup_dates) > 0) {
  warning("Panel skeleton: ", nrow(dup_dates), " duplicate player-date rows remain.")
} else {
  message("  QC: one-row-per-player-per-date invariant holds.")
}

message("  Panel: ", nrow(panel), " rows | Players: ", n_distinct(panel$PLAYER_ID),
        " | Played: ", sum(panel$PLAYED_FLAG),
        " | Missed: ", sum(panel$PLAYED_FLAG == 0))

# ── 3b. Player appearance metrics ── B2B, density flags, rolling minutes, ACWR, road trips
appeared <- panel %>%
  filter(PLAYED_FLAG == 1) %>%
  arrange(PLAYER_ID, GAME_DATE) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(
    PLAYER_DAYS_SINCE_LAST_PLAYED = as.numeric(GAME_DATE - lag(GAME_DATE)) - 1,
    IS_B2B     = as.integer(PLAYER_DAYS_SINCE_LAST_PLAYED == 0),
    IS_B2B_2ND = IS_B2B,
    IS_B2B_1ST = as.integer(lead(IS_B2B, default = 0L) == 1 & IS_B2B == 0),
    SEASON_B2B_COUNT       = cumsum(replace_na(IS_B2B, 0)),
    GAMES_PLAYED = row_number(),
    IS_3IN4 = as.integer(slide_index_dbl(rep(1, n()), GAME_DATE, sum,
                                         .before = days(3), .complete = FALSE) >= 3),
    IS_4IN5 = as.integer(slide_index_dbl(rep(1, n()), GAME_DATE, sum,
                                         .before = days(4), .complete = FALSE) >= 4),
    IS_5IN7 = as.integer(slide_index_dbl(rep(1, n()), GAME_DATE, sum,
                                         .before = days(6), .complete = FALSE) >= 5),
    ROLLING_MIN_7D  = slide_index_dbl(MIN, GAME_DATE, sum,
                                      .before = days(7),  .complete = FALSE),
    ROLLING_MIN_14D = slide_index_dbl(MIN, GAME_DATE, sum,
                                      .before = days(14), .complete = FALSE),
    ROLLING_MIN_28D = slide_index_dbl(MIN, GAME_DATE, sum,
                                      .before = days(28), .complete = FALSE),
    ROLLING_MIN_42D = slide_index_dbl(
      MIN, GAME_DATE, sum, .before = days(42), .complete = FALSE),
    
    SEASON_CUMULATIVE_MIN  = cumsum(MIN),
    
    # Rolling FREQUENCY / CONGESTION dose — I count the qualifying games in each
    # trailing window the same way I do the rolling minutes (sum over
    # .before = days(N), including the current game). I like these accumulated-dose
    # measures because they're the recency-weighted version of the point-in-time
    # IS_B2B flag and the season-long SEASON_B2B_COUNT — but unlike the point-in-time
    # flags they can't be fooled by a coach resting a player on a given night. They
    # tell me how much congestion the body has actually soaked up lately, which I
    # think is the cleaner exposure. (I did consider a rolling 3-in-4 dose but left
    # it out — it's just a coarse threshold restatement of short-term minutes
    # density that ROLLING_MIN_7D/28D already capture continuously.)
    ROLLING_GAMES_7D  = slide_index_dbl(rep(1, n()), GAME_DATE, sum,
                                        .before = days(7),  .complete = FALSE),
    ROLLING_GAMES_28D = slide_index_dbl(rep(1, n()), GAME_DATE, sum,
                                        .before = days(28), .complete = FALSE),
    ROLLING_B2B_7D    = slide_index_dbl(replace_na(IS_B2B, 0L), GAME_DATE, sum,
                                        .before = days(7),  .complete = FALSE),
    ROLLING_B2B_28D   = slide_index_dbl(replace_na(IS_B2B, 0L), GAME_DATE, sum,
                                        .before = days(28), .complete = FALSE),
    
    ACWR_MIN = case_when(
      ROLLING_MIN_28D > 0 & !is.na(ROLLING_MIN_28D) &
        slide_index_dbl(rep(1,n()), GAME_DATE, sum, .before=days(28)) >= 4
      ~ round(ROLLING_MIN_7D / (ROLLING_MIN_28D / 4), 3),
      TRUE ~ NA_real_
    ),
    
    ACWR_MIN_42 = case_when(
      ROLLING_MIN_42D > 0 ~ round(ROLLING_MIN_7D / (ROLLING_MIN_42D / 6), 3),
      TRUE ~ NA_real_
    ),
    ACWR_SPIKE = case_when(
      is.na(ACWR_MIN)  ~ "insufficient_data",
      ACWR_MIN > 1.5   ~ "high_risk",
      ACWR_MIN > 1.0   ~ "moderate",
      TRUE             ~ "low"
    ),
    player_away_run = {
      away <- IS_AWAY == 1
      rle_ <- rle(away)
      rep(seq_along(rle_$lengths) * rle_$values, rle_$lengths)
    },
    IS_PLAYER_ROAD_TRIP = as.integer(
      player_away_run > 0 &
        ave(player_away_run, player_away_run, FUN = length) >= 2
    ),
    PLAYER_ROAD_TRIP_GAME_NUMBER = if_else(
      IS_PLAYER_ROAD_TRIP == 1,
      ave(player_away_run, player_away_run, FUN = seq_along), 0
    )
  ) %>%
  select(-player_away_run) %>%
  ungroup()

panel <- panel %>%
  left_join(
    appeared %>% select(
      SEASON, PLAYER_ID, GAME_ID,
      PLAYER_DAYS_SINCE_LAST_PLAYED,
      IS_B2B, IS_B2B_1ST, IS_B2B_2ND, SEASON_B2B_COUNT,
      IS_3IN4, IS_4IN5, IS_5IN7, GAMES_PLAYED,
      ROLLING_MIN_7D, ROLLING_MIN_14D, ROLLING_MIN_28D, ROLLING_MIN_42D, SEASON_CUMULATIVE_MIN,
      ROLLING_GAMES_7D, ROLLING_GAMES_28D,
      ROLLING_B2B_7D, ROLLING_B2B_28D,
      ACWR_MIN, ACWR_MIN_42, ACWR_SPIKE,
      IS_PLAYER_ROAD_TRIP, PLAYER_ROAD_TRIP_GAME_NUMBER
    ),
    by = c("SEASON", "PLAYER_ID", "GAME_ID")
  )

# ── 3c. Rolling travel distance ── haversine between arenas, rolling + cumulative km
appearance_coords <- appeared %>%
  select(SEASON, PLAYER_ID, GAME_DATE, GAME_ID, ARENA_LAT, ARENA_LON) %>%
  arrange(SEASON, PLAYER_ID, GAME_DATE) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(
    PREV_LAT = lag(ARENA_LAT),
    PREV_LON = lag(ARENA_LON),
    PLAYER_TRAVEL_KM = case_when(
      is.na(PREV_LAT) | is.na(ARENA_LAT) ~ NA_real_,
      ARENA_LAT == PREV_LAT & ARENA_LON == PREV_LON ~ 0,
      TRUE ~ {
        R    <- 6371
        dlat <- (ARENA_LAT - PREV_LAT) * pi / 180
        dlon <- (ARENA_LON - PREV_LON) * pi / 180
        a    <- sin(dlat/2)^2 +
          cos(PREV_LAT * pi/180) * cos(ARENA_LAT * pi/180) * sin(dlon/2)^2
        R * 2 * atan2(sqrt(a), sqrt(1 - a))
      }
    ),
    ROLLING_DIST_7D  = slide_index_dbl(replace_na(PLAYER_TRAVEL_KM, 0),
                                       GAME_DATE, sum, .before = days(7),  .complete = FALSE),
    ROLLING_DIST_14D = slide_index_dbl(replace_na(PLAYER_TRAVEL_KM, 0),
                                       GAME_DATE, sum, .before = days(14), .complete = FALSE),
    ROLLING_DIST_28D = slide_index_dbl(replace_na(PLAYER_TRAVEL_KM, 0),
                                       GAME_DATE, sum, .before = days(28), .complete = FALSE),
    ROLLING_DIST_42D = slide_index_dbl(replace_na(PLAYER_TRAVEL_KM, 0),
                                       GAME_DATE, sum, .before = days(42), .complete = FALSE),
    SEASON_CUMULATIVE_DIST = cumsum(replace_na(PLAYER_TRAVEL_KM, 0))
  ) %>%
  ungroup() %>%
  select(SEASON, PLAYER_ID, GAME_ID,
         PLAYER_TRAVEL_KM, ROLLING_DIST_7D, ROLLING_DIST_14D, ROLLING_DIST_28D, ROLLING_DIST_42D, SEASON_CUMULATIVE_DIST)

panel <- panel %>%
  left_join(appearance_coords, by = c("SEASON", "PLAYER_ID", "GAME_ID"))

# ── 3d. Game load tracking ── join the in-game movement metrics onto the panel.
panel <- panel %>%
  left_join(
    clean_game_load %>%
      select(-any_of(c("TEAMID","TEAMTRICODE","MINUTES",
                       "PLUSMINUSPOINTS","FOULSPERSONAL","SEASON"))) %>%
      # I force this to be a 1:1 join. clean_game_load has a few duplicate
      # (PLAYER_ID, GAME_ID) rows, and if I don't dedupe them they multiply my panel
      # rows (and anything I roll up from them), which would break the one-row-per-
      # player-game rule the counting-process model relies on.
      distinct(PLAYER_ID, GAME_ID, .keep_all = TRUE),
    by = c("PLAYER_ID", "GAME_ID")
  )

# ── 3d.1 Tracking metrics are only real on games the player PLAYED ──
# I found a nasty bug here. clean_game_load carries DNP rows with 0s — a player who
# shows up in the tracking box score but didn't really play — and left-joining those
# onto a missed game attaches DISTANCE = 0 (not NA). Downstream in file 5 the carry-
# forward (LOCF) then PROPAGATES that 0 forward instead of the last real played value,
# so injured players' rows end up with a fake ~0 distance. MIN dodges this because it's
# always NA (never 0) on a missed game, so its carry-forward is clean. The fix: blank
# the in-game tracking metrics on non-played rows so they're NA like MIN, and the
# carry-forward uses the last game the player actually played.
tracking_cols <- c("DISTANCE", "SPEED", "TOUCHES", "USAGEPERCENTAGE", "POSSESSIONS", "PACE")
panel <- panel %>%
  mutate(across(any_of(tracking_cols),
                ~ if_else(PLAYED_FLAG == 1, as.numeric(.x), NA_real_)))

# ── 3d.2 Rolling ON-COURT physical load (external load) ── built from tracking DISTANCE
# My reasoning: minutes played is really a COACHING decision, so it's selection-biased.
# The DISTANCE a player covers on court is closer to what their body actually absorbed
# — what sports science calls "external load". So I roll distance into the same acute
# (7d) and chronic (28d) windows I use for minutes, and build a tracking-based ACWR
# (acute:chronic ratio), which is the version of ACWR the literature actually intends.
tracking_load <- panel %>%
  filter(PLAYED_FLAG == 1) %>%                                  # load only exists on games he played
  arrange(SEASON, PLAYER_ID, GAME_DATE) %>%                     # chronological, per player
  group_by(SEASON, PLAYER_ID) %>%                              # windows reset each player-season
  mutate(
    ROLLING_LOAD_7D = slide_index_dbl(                          # sum of distance over trailing 7 days
      replace_na(DISTANCE, 0), GAME_DATE, sum,                  # treat a missing reading as 0
      .before = days(7), .complete = FALSE),                    # inclusive of today; don't require a full window
    ROLLING_LOAD_28D = slide_index_dbl(                         # same over trailing 28 days = chronic load
      replace_na(DISTANCE, 0), GAME_DATE, sum,
      .before = days(28), .complete = FALSE),
    ACWR_LOAD = case_when(                                      # acute:chronic external-load ratio
      ROLLING_LOAD_28D > 0 &                                    # need a real chronic base, and...
        slide_index_dbl(rep(1, n()), GAME_DATE, sum,            # ...at least 4 games in the 28d window
                        .before = days(28)) >= 4                # (burn-in guard, mirrors ACWR_MIN)
      ~ round(ROLLING_LOAD_7D / (ROLLING_LOAD_28D / 4), 3),     # weekly acute vs weekly-average chronic
      TRUE ~ NA_real_)                                          # otherwise undefined (too few games)
  ) %>%
  ungroup() %>%
  select(SEASON, PLAYER_ID, GAME_ID,                            # keep only keys + the new features
         ROLLING_LOAD_7D, ROLLING_LOAD_28D, ACWR_LOAD)

panel <- panel %>%                                              # attach the rolling-load features back
  left_join(tracking_load, by = c("SEASON", "PLAYER_ID", "GAME_ID"))

# ── 3d.3 On-court INTENSITY (load per minute) ── the part of load that ISN'T volume.
# I found that rolling DISTANCE and rolling MINUTES are ~0.99 collinear — a player's
# distance is basically their minutes times a fairly steady rate — so rolling distance
# on its own can't tell me anything minutes don't. What actually SEPARATES them is
# intensity: distance covered PER minute, i.e. how hard they moved rather than how long.
# I build it per game (DISTANCE / MIN) and as a trailing-window average (total distance
# / total minutes over the window). The window version is decorrelated from minutes
# (~ -0.27), so it lets me carry a genuine on-court-load signal alongside minutes as the
# volume control, instead of a distance term that's just minutes in disguise.
# I only trust the rolling ratio once there's a real base of minutes in the window
# (>=30 in 7d, >=60 in 28d); below that the ratio is dividing tiny numbers and is noise.
panel <- panel %>%
  mutate(
    INTENSITY = if_else(!is.na(MIN) & MIN > 0 & !is.na(DISTANCE), DISTANCE / MIN, NA_real_),
    ROLLING_INTENSITY_7D  = if_else(ROLLING_MIN_7D  >= 30, ROLLING_LOAD_7D  / ROLLING_MIN_7D,  NA_real_),
    ROLLING_INTENSITY_28D = if_else(ROLLING_MIN_28D >= 60, ROLLING_LOAD_28D / ROLLING_MIN_28D, NA_real_)
  )

# ── 3e. Career & bio context ── join player bio, career totals, preseason GP, derive age
panel <- panel %>%
  left_join(
    clean_player_bio %>%
      select(PLAYER_ID, BIRTHDATE, POSITION, HEIGHT, HEIGHT_INCHES, WEIGHT,
             COUNTRY, DRAFT_YEAR, DRAFT_ROUND, DRAFT_NUMBER),
    by = "PLAYER_ID"
  ) %>%
  left_join(clean_career, by = c("SEASON", "PLAYER_ID")) %>%
  left_join(preseason_participation, by = c("SEASON", "PLAYER_ID")) %>%
  mutate(
    PRESEASON_GP       = replace_na(PRESEASON_GP, 0L),
    # My proxy for "did this player have a normal training camp?" is simply whether
    # they played in ANY pre-season game. The raw count is too noisy to trust
    # (coaches rest their starters and run deep benches), so the yes/no flag —
    # showed up vs late signing / camp absentee — is the cleaner signal. I keep the
    # PRESEASON_GP count around for EDA.
    PLAYED_PRESEASON   = as.integer(PRESEASON_GP > 0),
    PLAYER_AGE_AT_GAME = round(as.numeric(GAME_DATE - BIRTHDATE) / 365.25, 2)
  ) %>%
  select(-BIRTHDATE)

# ── 3f. Injury join ── attach the injury reports and derive the timeloss flags and
# body region.
# I join on my NORMALISED name key rather than the raw name, because otherwise the
# spelling drift between the injury reports and the game logs (Jokic vs Jokić, Butler
# vs Butler III) silently drops real injuries — which was the whole point of the
# audit. I also dedupe the injury side on that key first, to cover the rare case
# where two different players collapse to the same key on one date (I keep the
# genuine Out).
injuries_join <- clean_injuries %>%
  mutate(join_key = norm_name(PLAYER_NAME_NORM)) %>%
  arrange(SEASON, game_date, join_key, desc(IS_OUT), desc(IS_INJURY_ABSENCE)) %>%
  distinct(SEASON, game_date, join_key, .keep_all = TRUE) %>%
  select(
    SEASON, GAME_DATE = game_date, join_key,
    INJURY_CATEGORY   = injury_category,
    INJURY_REASON     = reason,
    IS_OUT, IS_QUESTIONABLE, IS_PROBABLE,
    IS_INJURY_ABSENCE, IS_STRUCTURAL
  )

panel <- panel %>%
  mutate(join_key = norm_name(PLAYER_NAME)) %>%
  left_join(injuries_join, by = c("SEASON", "GAME_DATE", "join_key")) %>%
  mutate(
    INJURY_FLAG     = as.integer(!is.na(INJURY_CATEGORY)),
    INJURY_TIMELOSS = case_when(
      PLAYED_FLAG == 0 & IS_INJURY_ABSENCE == 1 ~ 1,
      PLAYED_FLAG == 1 ~ 0,
      TRUE ~ 0
    ),
    # Body region (from my rulebook) so I can spot reaggravations of the same part.
    INJURY_BODY_REGION = injury_body_region(INJURY_CATEGORY),
    # Mechanism tier for my negative-control outcome: soft_tissue (overuse, load
    # SHOULD drive) vs traumatic_load vs random_impact (contact, the clean control).
    INJURY_MECHANISM = injury_mechanism(INJURY_CATEGORY)
  ) %>%
  select(-join_key, -IS_OUT, -IS_INJURY_ABSENCE)

# ── 3g. Injury episode construction ── this is the fiddliest part of the whole file.
# A player can miss a run of games for one injury, so I don't want to count every
# missed game as a new event. Instead I group consecutive absences into an "episode",
# then decide whether each episode is a brand-NEW injury or a REAGGRAVATION of the
# same body part within 90 days. That new-vs-reaggravation split is what feeds my two
# event definitions later.
message(" [3g] Building injury episode variables ...")

panel <- panel %>%
  arrange(SEASON, PLAYER_ID, GAME_DATE) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(
    prev_played      = lag(PLAYED_FLAG, default = 1L),
    prev_body_region = lag(INJURY_BODY_REGION, default = NA_character_),
    
    # I say a new spell starts either on the first missed game after a played one,
    # OR when the body region changes mid-absence (e.g. it flips back -> foot without
    # a game in between, which I read as a second injury stacked on the first).
    region_changed = as.integer(
      INJURY_TIMELOSS == 1 &
        prev_played == 0 &
        !is.na(INJURY_BODY_REGION) &
        !is.na(prev_body_region) &
        INJURY_BODY_REGION != "unspecified" &
        prev_body_region  != "unspecified" &
        INJURY_BODY_REGION != prev_body_region
    ),
    
    spell_start = as.integer(
      (INJURY_TIMELOSS == 1 & prev_played == 1) | region_changed == 1
    ),
    
    INJURY_EPISODE_ID = if_else(
      INJURY_TIMELOSS == 1,
      cumsum(spell_start),
      NA_integer_
    ),
    
    TIMELOSS_GAMES = if_else(
      INJURY_TIMELOSS == 1,
      ave(INJURY_TIMELOSS, cumsum(prev_played == 1), FUN = cumsum),
      0L
    ),
    
    last_played_date = zoo::na.locf(
      if_else(PLAYED_FLAG == 1, GAME_DATE, NA_Date_),
      na.rm = FALSE
    ),
    
    # Region at each spell start, carried forward across the episode
    episode_region = zoo::na.locf(
      if_else(spell_start == 1, INJURY_BODY_REGION, NA_character_),
      na.rm = FALSE
    ),
    
    DAYS_SINCE_LAST_RETURN = if_else(
      INJURY_TIMELOSS == 1 & spell_start == 1,
      as.numeric(GAME_DATE - lag(last_played_date, default = NA_Date_)),
      NA_real_
    ),
    
    # Episode region at spell start, and the RETURN date (first played game
    # after a timeloss spell) carried backward to fill the episode it belongs to.
    ep_region    = if_else(spell_start == 1 & !is.na(episode_region), episode_region, NA_character_),
    ep_return_raw = if_else(
      PLAYED_FLAG == 1 & lag(INJURY_TIMELOSS, default = 0L) == 1,
      GAME_DATE,
      NA_Date_
    ),
    ep_return = zoo::na.locf(ep_return_raw, fromLast = TRUE, na.rm = FALSE),
    
    # For each spell start I look back over ALL earlier same-region episodes this
    # season and measure how many days it's been since the most recent same-region
    # return. This is what lets me call something a reaggravation below.
    DAYS_SINCE_LAST_SAME_REGION_RETURN = {
      regions <- ep_region
      returns <- ep_return
      cur_reg <- episode_region
      n       <- n()
      result  <- rep(NA_real_, n)
      
      for (i in seq_len(n)) {
        if (!isTRUE(spell_start[i] == 1) || is.na(cur_reg[i]) ||
            cur_reg[i] == "unspecified") next
        
        prior_same <- which(
          seq_len(n) < i &
            !is.na(regions) &
            regions == cur_reg[i]
        )
        if (length(prior_same) == 0) next
        
        prior_same_with_date <- prior_same[!is.na(returns[prior_same])]
        if (length(prior_same_with_date) == 0) next
        
        most_recent_idx <- prior_same_with_date[
          which.max(returns[prior_same_with_date])
        ]
        result[i] <- as.numeric(GAME_DATE[i] - returns[most_recent_idx])
      }
      result
    }
  ) %>%
  mutate(
    # I flag a reaggravation when it's the same region within 90 days (using that
    # look-back scan), or when it's an unspecified absence within 90 days — I treat
    # the unspecified case as a reaggravation too, to be conservative about not
    # over-counting brand-new injuries.
    IS_REAGGRAVATION = as.integer(
      spell_start == 1 &
        INJURY_TIMELOSS == 1 &
        (
          (!is.na(DAYS_SINCE_LAST_SAME_REGION_RETURN) &
             DAYS_SINCE_LAST_SAME_REGION_RETURN <= 90) |
            (coalesce(episode_region, "") == "unspecified" &
               !is.na(DAYS_SINCE_LAST_RETURN) &
               DAYS_SINCE_LAST_RETURN <= 90)
        )
    ),
    
    IS_NEW_INJURY = as.integer(
      spell_start == 1 &
        INJURY_TIMELOSS == 1 &
        IS_REAGGRAVATION == 0
    ),
    
    REAG_CUMSUM       = cumsum(IS_REAGGRAVATION),
    NEW_INJURY_CUMSUM = cumsum(IS_NEW_INJURY),
    TIMELOSS_CUMSUM   = cumsum(INJURY_TIMELOSS)
  ) %>%
  # Drop episode-construction working columns
  select(
    -prev_played, -prev_body_region, -region_changed, -spell_start,
    -last_played_date, -episode_region, -ep_region, -ep_return, -ep_return_raw
  ) %>%
  ungroup()

message("  Injury episodes built")
message("    Total timeloss game-rows:   ",
        sum(panel$INJURY_TIMELOSS == 1, na.rm = TRUE))
message("    Distinct injury episodes:   ",
        sum(panel$IS_NEW_INJURY == 1, na.rm = TRUE) +
          sum(panel$IS_REAGGRAVATION == 1, na.rm = TRUE))
message("    New injury events:          ",
        sum(panel$IS_NEW_INJURY == 1, na.rm = TRUE))
message("    Reaggravation events:       ",
        sum(panel$IS_REAGGRAVATION == 1, na.rm = TRUE))

# ── 3h. Cumulative games missed (within season) ── a time-varying fragility signal.
# This is a running count of how many games the player has missed SO FAR this season
# as they walk into each game. I have to build it HERE, on the full panel while every
# scheduled game is still a row — file 5 drops the injured-absence rows for the risk
# set, so if I waited it'd be too late. The lag() makes it "games missed BEFORE this
# game", which keeps it exogenous to whatever happens in the current row.
panel <- panel %>%
  arrange(SEASON, PLAYER_ID, GAME_DATE) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(
    SEASON_GAMES_MISSED = lag(cumsum(PLAYED_FLAG == 0), default = 0L)
  ) %>%
  ungroup()

# ── 3h.2 Prior-injury history (within-season, time-varying) ── the recurrent burden.
# These are the covariates I added to close the "prior injury isn't in the model"
# gap. For each game I encode what the player's injury history ALREADY is as they
# enter it. Like SEASON_GAMES_MISSED above, they're all lagged / past-only and built
# on the full panel (injured rows still present), so they stay exogenous to the
# current row's event.
#   PRIOR_EPISODES_SEASON  how many injury episodes (new + reaggravation) they've had
#                          this season already (recurrent burden; Martin 2021)
#   EVER_INJURED_SEASON    have they had at least one episode this season yet
#   RECENT_RETURN          did they come back from an absence in the last N days —
#                          the acute re-injury window (Fulton 2014). This is the
#                          "returns straight into a heavy stretch" pattern.
RECENT_RETURN_DAYS <- 14L

panel <- panel %>%
  arrange(SEASON, PLAYER_ID, GAME_DATE) %>%
  group_by(SEASON, PLAYER_ID) %>%
  mutate(
    # Episodes entering this game = lag of the running episode count (new + reaggr).
    PRIOR_EPISODES_SEASON = lag(
      cumsum(replace_na(IS_NEW_INJURY, 0L) + replace_na(IS_REAGGRAVATION, 0L)),
      default = 0L
    ),
    EVER_INJURED_SEASON = as.integer(PRIOR_EPISODES_SEASON > 0),
    # Date of the most recent return-to-play after an injury absence, carried
    # forward. A return game is a played game whose previous row was timeloss.
    .return_date = if_else(
      PLAYED_FLAG == 1 & lag(INJURY_TIMELOSS, default = 0L) == 1,
      GAME_DATE, NA_Date_
    ),
    .last_return = zoo::na.locf(.return_date, na.rm = FALSE),
    RECENT_RETURN = as.integer(
      !is.na(.last_return) &
        as.numeric(GAME_DATE - .last_return) <= RECENT_RETURN_DAYS
    )
  ) %>%
  select(-.return_date, -.last_return) %>%
  ungroup()

message("  Prior-injury covariates built | recent-return game-rows: ",
        sum(panel$RECENT_RETURN, na.rm = TRUE),
        " | rows entering with >=1 prior episode: ",
        sum(panel$EVER_INJURED_SEASON, na.rm = TRUE))

# ── 3i. Season structure & training-context flags ── season phase, % through season,
# post-All-Star.
# I anchor the phase boundaries to the season's own structure (days into the season
# and the All-Star break) rather than calendar months, so "early / mid / late" means
# the same thing every season even when the calendar shifts.
allstar_dates <- tibble::tribble(
  ~SEASON,      ~ALLSTAR_DATE,
  "2021-22",    as.Date("2022-02-20"),
  "2022-23",    as.Date("2023-02-19"),
  "2023-24",    as.Date("2024-02-18"),
  "2024-25",    as.Date("2025-02-16"),
  "2025-26",    as.Date("2026-02-15")   # verify final 2025-26 date
)

panel <- panel %>%
  left_join(allstar_dates, by = "SEASON") %>%
  group_by(SEASON) %>%
  mutate(
    SEASON_START       = min(GAME_DATE, na.rm = TRUE),
    SEASON_END         = max(GAME_DATE, na.rm = TRUE),
    DAYS_INTO_SEASON   = as.numeric(GAME_DATE - SEASON_START),
    SEASON_LENGTH_DAYS = as.numeric(SEASON_END - SEASON_START),
    PCT_THROUGH_SEASON = round(DAYS_INTO_SEASON / SEASON_LENGTH_DAYS, 4),
    IS_POST_ALLSTAR    = as.integer(GAME_DATE > ALLSTAR_DATE)
  ) %>%
  ungroup() %>%
  mutate(
    # early = first ~6 weeks (camp residual, settling); mid = up to All-Star;
    # late = post All-Star run-in (congestion, playoff push)
    season_phase = factor(case_when(
      DAYS_INTO_SEASON <= 42                       ~ "early",
      DAYS_INTO_SEASON > 42 & IS_POST_ALLSTAR == 0 ~ "mid",
      IS_POST_ALLSTAR == 1                         ~ "late",
      TRUE ~ NA_character_
    ), levels = c("early", "mid", "late"))
  )

# ── 3j. panel ── lean modelling-bound subset consumed by Modelling Prep
panel <- panel %>%
  select(
    # Identity
    SEASON, GAME_ID, GAME_DATE, PLAYER_ID, PLAYER_NAME,
    TEAM_ID, TEAM_TRICODE, TEAM_ABBREVIATION,
    # Game context
    IS_HOME, IS_AWAY, OPPONENT_TRICODE, weekNumber, monthNum,
    season_phase, DAYS_INTO_SEASON, PCT_THROUGH_SEASON, IS_POST_ALLSTAR,
    # Outcome
    PLAYED_FLAG, INJURY_FLAG, INJURY_CATEGORY, INJURY_REASON, INJURY_BODY_REGION,
    INJURY_MECHANISM,
    INJURY_TIMELOSS, INJURY_EPISODE_ID, TIMELOSS_GAMES,
    IS_NEW_INJURY, IS_REAGGRAVATION, IS_QUESTIONABLE, IS_PROBABLE, IS_STRUCTURAL,
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
    # Team schedule stress
    TEAM_DAYS_REST, TEAM_IS_B2B, TEAM_TRAVEL_KM, TEAM_IS_LATE_GAME,
    TEAM_IS_ROAD_TRIP, TEAM_ROAD_TRIP_GAME_NUMBER,
    TEAM_ROAD_TRIP_LENGTH, TEAM_CUMULATIVE_ROAD_TRIP_KM,
    # Player appearance schedule stress
    IS_B2B, IS_B2B_1ST, IS_B2B_2ND, SEASON_B2B_COUNT,
    IS_3IN4, IS_4IN5, IS_5IN7,
    ROLLING_B2B_7D, ROLLING_B2B_28D,
    IS_PLAYER_ROAD_TRIP, PLAYER_ROAD_TRIP_GAME_NUMBER,
    # Opponent context
    OPPONENT_DAYS_REST, OPPONENT_IS_B2B, OPPONENT_IS_ROAD_TRIP,
    # Box score
    MIN, PTS, REB, AST, TOV, PLUS_MINUS,
    # Tracking load
    DISTANCE, SPEED, TOUCHES, USAGEPERCENTAGE, POSSESSIONS, PACE,
    # Player physical context
    PLAYER_AGE_AT_GAME, POSITION, HEIGHT_INCHES, WEIGHT, DRAFT_YEAR, DRAFT_NUMBER,
    # Career context
    CAREER_GAMES, CAREER_MINUTES, CAREER_GAMES_MISSED, SEASONS_IN_LEAGUE,
    PLAYED_PRESEASON,
    PRIOR_SEASON_GP, PRIOR_SEASON_MIN_PG, PRIOR_SEASON_PTS
  )

saveRDS(panel, file.path(BASE, "panel_2021-26.rds"))

message("\n=== Panel complete === rows: ", nrow(panel), " | cols: ", ncol(panel),
        " | players: ", n_distinct(panel$PLAYER_ID))

# =============================================================================
# 4. AUDIT — do my injury reports actually land on a panel row?
# =============================================================================
# My panel is only as good as its OUTCOME, so I check the outcome before I trust
# anything built on it. Every injury gets in through a name-based join (the report
# gives me a NAME, the game logs give me a PLAYER_ID), which means a silent name
# mismatch would drop real injuries and quietly relabel those player-games as
# healthy. That's the worst mistake this script could make — it would drag my event
# count down, and because the mismatches cluster on the transient players it'd do it
# non-randomly.
#
# So I measure it directly: of the genuine time-loss (Out) reports on real
# regular-season dates, what share find a matching (season, date, player) row in my
# panel? This is the number that convinced me to rebuild the skeleton from game logs
# back in 3a.
message("\n--- AUDIT: injury-report -> panel match rate ---")

# I only count genuine time-loss reports on actual regular-season dates, so my
# denominator is just the reports that SHOULD have a panel row to land on in the
# first place — otherwise I'd be penalising myself for reports that were never in
# scope.
reg_dates <- clean_sched %>% transmute(k = paste(SEASON, GAME_DATE)) %>% distinct()

audit_reports <- clean_injuries %>%
  filter(IS_INJURY_ABSENCE == 1, IS_OUT == 1,                 # genuine time-loss, not rest/personal
         paste(SEASON, game_date) %in% reg_dates$k) %>%       # on a real regular-season date
  mutate(key = paste(SEASON, game_date, norm_name(PLAYER_NAME_NORM)))

audit_panel_keys <- panel %>%
  transmute(key = paste(SEASON, GAME_DATE, norm_name(PLAYER_NAME))) %>%
  distinct()

audit_match_rate <- mean(audit_reports$key %in% audit_panel_keys$key)
message("Genuine time-loss injury reports (regular season): ", nrow(audit_reports))
message("Injury-report -> panel match rate: ", round(100 * audit_match_rate, 1), "%")
message("(A low rate here means injuries are being LOST to name mismatches and",
        " those player-games are wrongly counted as healthy.)")

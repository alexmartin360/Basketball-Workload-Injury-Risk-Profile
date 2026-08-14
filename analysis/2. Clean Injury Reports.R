
# 2. Clean Injury Reports
# Turns the raw scraped report filings into one clean row per player per game date.
# The NBA files a fresh report every 30 minutes before tip-off, so I keep the LAST
# filing for each player-game, classify its reason, and flag genuine injury absences.
# Input:   data/raw/nba_injuries/injury_reports/*.parquet
# Output:  data/processed/clean_injuries.rds

library(tidyverse)
library(arrow)
library(fs)

SEASONS <- c("2021-22", "2022-23", "2023-24", "2024-25", "2025-26")
OUT_DIR <- here::here("data", "processed")
dir.create(OUT_DIR, showWarnings = FALSE)

source(here::here("analysis", "1. Injury Classification Rules.R"))

message("=== NBA Injury Report Cleaning ===")

# ── 1. Load every report the scraper saved ──
injury_files <- dir_ls(here::here("data", "raw", "nba_injuries", "injury_reports"),
                       recurse = TRUE, glob = "*.parquet")
message("[1] Reading ", length(injury_files), " parquet files ...")

# Everything is read as character: the empty report files come back with different
# column types, which makes stacking them fail. Types are fixed straight after.
raw_injuries <- injury_files %>%
  map_dfr(~ read_parquet(.x) %>% mutate(across(everything(), as.character))) %>%
  rename(game_date = `Game Date`, player_name = `Player Name`,
         current_status = `Current Status`, reason = Reason) %>%
  mutate(game_date        = as.Date(game_date, format = "%m/%d/%Y"),
         report_timestamp = as.POSIXct(report_timestamp)) %>%
  filter(!is.na(game_date), !is.na(player_name), !str_trim(player_name) %in% c("", "nan")) %>%
  distinct()

message("  Raw rows: ", nrow(raw_injuries),
        " | ", min(raw_injuries$game_date), " to ", max(raw_injuries$game_date))

# ── 2. Assign a season, keep the final pre-game filing, classify ──
# A season runs October to September, so games from October on belong to the season
# starting that year and everything else to the one before.
message("\n[2] Resolving to one row per player-game ...")

clean_injuries <- raw_injuries %>%
  mutate(start_year = if_else(month(game_date) >= 10,
                              year(game_date), year(game_date) - 1L),
         SEASON = paste0(start_year, "-", str_sub(start_year + 1L, 3, 4))) %>%
  filter(SEASON %in% SEASONS) %>%
  # Newest filing first, then keep one row per player-game: the last word before tip-off.
  arrange(game_date, player_name, desc(report_timestamp)) %>%
  distinct(game_date, player_name, .keep_all = TRUE) %>%
  mutate(
    injury_category = classify_injury(tolower(coalesce(reason, ""))),
    # Reports write names "Last, First"; the game logs use "First Last".
    PLAYER_NAME_NORM = str_trim(str_replace(player_name, "^([^,]+),\\s*(.*)$", "\\2 \\1")),
    IS_OUT            = current_status %in% c("Out", "Doubtful"),
    IS_QUESTIONABLE   = current_status == "Questionable",
    IS_PROBABLE       = current_status == "Probable",
    IS_INJURY_ABSENCE = is_injury_absence_flag(injury_category),
    IS_STRUCTURAL     = is_structural_flag(injury_category)
  ) %>%
  select(SEASON, game_date, PLAYER_NAME_NORM, injury_category, reason,
         IS_OUT, IS_QUESTIONABLE, IS_PROBABLE, IS_INJURY_ABSENCE, IS_STRUCTURAL)

clean_injuries %>%
  count(SEASON, name = "player_date_rows") %>%
  left_join(clean_injuries %>% filter(IS_OUT) %>% count(SEASON, name = "out_doubtful"),
            by = "SEASON") %>%
  print()

# ── 3. Save ──
saveRDS(clean_injuries, file.path(OUT_DIR, "clean_injuries.rds"))
message("\n=== Injury cleaning complete === rows: ", nrow(clean_injuries),
        " | injury absences: ", sum(clean_injuries$IS_INJURY_ABSENCE))

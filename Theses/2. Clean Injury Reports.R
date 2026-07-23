# =============================================================================
# 2. Clean Injury Reports
# =============================================================================
# This is where I turn the raw scraped injury-report filings into one clean row
# per player per game date. The NBA files a fresh report every 30 minutes before
# tip-off, so for any given player-game I have several filings; my job here is to
# keep the LAST one (the final word before the game), classify its reason with my
# rulebook, and flag whether it's a genuine injury absence.
#
# I pull the classification rules in from file 1 so I'm not duplicating them.
# Input:   nba_injuries/injury_reports/*.parquet  (the 30-min pre-game filings)
# Output:  data_processed/clean_injuries.rds
# =============================================================================

library(tidyverse)
library(arrow)
library(fs)

# ── Config ──
SEASONS <- c("2021-22", "2022-23", "2023-24", "2024-25", "2025-26")

BASE_PATH   <- "/Users/alexmartin/Library/Mobile Documents/com~apple~CloudDocs/Filing Cabinet/University/Latrobe/SPA-A/Project Code Base/Basketball-Workload-Injury-Risk-Profile"
BASE_PATH_T <- file.path(BASE_PATH, "Theses")
OUT_DIR     <- file.path(BASE_PATH, "data_processed")
dir.create(OUT_DIR, showWarnings = FALSE)

source(file.path(BASE_PATH_T, "1. Injury Classification Rules.R"))

message("=== NBA Injury Report Cleaning ===")
message("    Seasons: ", paste(SEASONS, collapse = ", "))

# STEP 1 — load every parquet the scraper saved, tidy the column names, drop the
# empty rows. There are a lot of small files (one per report), so I read them all
# and stack them.
message("\n[1] Loading injury report parquet files ...")

injury_dir   <- file.path(BASE_PATH, "nba_injuries", "injury_reports")
injury_files <- fs::dir_ls(injury_dir, recurse = TRUE, glob = "*.parquet")
message("  Found ", length(injury_files), " parquet files")

if (length(injury_files) == 0) {
  stop("No injury report files found in: ", injury_dir,
       "\nRun nba_injury_report_scraper.py first.")
}

# I cast every column to character as I read it. This bit me early on: the empty
# report files come back with different column types, and that makes bind_rows
# blow up when I stack the whole set. Forcing everything to character sidesteps it,
# and I fix the types up properly straight after.
raw_injuries_all <- injury_files %>%
  map_dfr(~ {
    tryCatch(
      arrow::read_parquet(.x) %>% mutate(across(everything(), as.character)),
      error = function(e) {
        message("  WARNING: skipping ", basename(.x), " — ", e$message)
        NULL
      }
    )
  }) %>%
  rename(
    game_date      = `Game Date`,
    player_name    = `Player Name`,
    current_status = `Current Status`,
    reason         = Reason,
  ) %>%
  mutate(
    game_date        = as.Date(game_date, format = "%m/%d/%Y"),
    report_timestamp = as.POSIXct(report_timestamp),
  ) %>%
  filter(
    !is.na(player_name),
    str_trim(player_name) != "nan",
    str_trim(player_name) != "",
    !is.na(game_date)
  ) %>%
  distinct()

message("  Raw rows loaded: ", nrow(raw_injuries_all),
        " | date range: ", min(raw_injuries_all$game_date, na.rm = TRUE),
        " to ",            max(raw_injuries_all$game_date, na.rm = TRUE))

# STEP 2 — for each season I classify the reason, tidy the player name, and resolve
# down to the single final pre-game report per player per game date.
message("\n[2] Classifying and resolving per season ...")

season_starts <- as.Date(paste0(str_sub(SEASONS, 1, 4), "-10-01"))
season_ends   <- as.Date(paste0(as.integer(str_sub(SEASONS, 1, 4)) + 1, "-09-30"))
injuries_list <- list()

for (i in seq_along(SEASONS)) {
  
  SEASON  <- SEASONS[i]
  s_start <- season_starts[i]
  s_end   <- season_ends[i]
  
  inj_season <- raw_injuries_all %>%
    filter(game_date >= s_start, game_date <= s_end) %>%
    mutate(
      SEASON          = SEASON,
      reason_lower    = tolower(coalesce(reason, "")),
      injury_category = classify_injury(reason_lower),
      # The reports write names "Last, First" but my game logs use "First Last", so
      # I flip them here to give the panel join a fighting chance.
      PLAYER_NAME_NORM = case_when(
        str_detect(player_name, ",") ~ str_trim(paste(
          str_trim(str_extract(player_name, "(?<=,\\s?).*")),
          str_trim(str_extract(player_name, "^[^,]+"))
        )),
        TRUE ~ str_trim(player_name)
      )
    ) %>%
    select(-reason_lower) %>%
    # I only want the FINAL report for each player-game, so I sort newest-first by
    # timestamp and keep the first row — the latest filing before tip-off wins.
    arrange(game_date, player_name, desc(report_timestamp)) %>%
    distinct(game_date, player_name, .keep_all = TRUE) %>%
    mutate(
      IS_OUT            = current_status %in% c("Out", "Doubtful"),
      IS_QUESTIONABLE   = current_status == "Questionable",
      IS_PROBABLE       = current_status == "Probable",
      IS_INJURY_ABSENCE = is_injury_absence_flag(injury_category),
      IS_STRUCTURAL     = is_structural_flag(injury_category),
    )
  
  injuries_list[[SEASON]] <- inj_season
  
  message(
    "  [", SEASON, "] ",
    nrow(inj_season),               " player-date rows | ",
    "out/doubtful: ",  sum(inj_season$IS_OUT),            " | ",
    "injury absence: ", sum(inj_season$IS_INJURY_ABSENCE), " | ",
    "structural: ",    sum(inj_season$IS_STRUCTURAL)
  )
}

# STEP 3 — stack the seasons back together, keep only the columns I actually use
# downstream, and save.
message("\n[3] Stacking and saving ...")

clean_injuries <- bind_rows(injuries_list) %>%
  select(
    SEASON, game_date, PLAYER_NAME_NORM,
    injury_category, reason,
    IS_OUT, IS_QUESTIONABLE, IS_PROBABLE,
    IS_INJURY_ABSENCE, IS_STRUCTURAL
  )

# A quick sanity check for myself — print how the Out/Doubtful absences split across
# my categories, so I can eyeball whether the classification looks sensible.
cat_summary <- clean_injuries %>%
  filter(IS_OUT) %>%
  count(injury_category, sort = TRUE) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

message("\n  Category distribution (Out/Doubtful only):")
message(sprintf("  %-30s %6s %6s", "Category", "n", "%"))
message("  ", strrep("-", 45))
for (j in seq_len(nrow(cat_summary))) {
  message(sprintf("  %-30s %6d %5.1f%%",
                  cat_summary$injury_category[j],
                  cat_summary$n[j],
                  cat_summary$pct[j]))
}

saveRDS(clean_injuries, file.path(OUT_DIR, "clean_injuries.rds"))
write_csv(clean_injuries, file.path(OUT_DIR, "clean_injuries.csv"))

message("\n  Saved: ", nrow(clean_injuries), " rows x ",
        ncol(clean_injuries), " cols")
message("  -> ", file.path(OUT_DIR, "clean_injuries.rds"))
message("\n=== Injury cleaning complete ===")
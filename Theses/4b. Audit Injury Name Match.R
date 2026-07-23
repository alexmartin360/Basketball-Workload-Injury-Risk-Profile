# =============================================================================
# 4b. Audit Injury Name Match  (data-quality check)
# =============================================================================
# Confirms how many official (Out) injury reports successfully land on a panel row
# after the name normalisation applied in file 4. This is the evidence that the
# injury OUTCOME is being captured.
# =============================================================================

library(tidyverse)
library(stringi)

BASE <- "~/Library/Mobile Documents/com~apple~CloudDocs/Filing Cabinet/University/Latrobe/SPA-A/Project Code Base/Basketball-Workload-Injury-Risk-Profile/data_processed"

inj   <- readRDS(file.path(BASE, "clean_injuries.rds"))
panel <- readRDS(file.path(BASE, "panel_2021-26.rds"))
sched <- readRDS(file.path(BASE, "clean_sched.rds"))

# Same deterministic name normaliser used for the join in file 4.
norm_name <- function(x) {
  x <- stri_trans_general(x, "Latin-ASCII"); x <- tolower(x)
  x <- str_replace_all(x, "[.']", ""); x <- str_replace_all(x, "-", " ")
  x <- str_replace_all(x, "\\b(jr|sr|ii|iii|iv|v)\\b", ""); str_squish(x)
}

# Restrict to genuine time-loss reports on actual regular-season game dates.
reg <- sched %>% transmute(k = paste(SEASON, GAME_DATE)) %>% distinct()
gi <- inj %>%
  filter(IS_INJURY_ABSENCE == 1, IS_OUT == 1,
         paste(SEASON, game_date) %in% reg$k) %>%
  mutate(key = paste(SEASON, game_date, norm_name(PLAYER_NAME_NORM)))

panel_keys <- panel %>%
  transmute(key = paste(SEASON, GAME_DATE, norm_name(PLAYER_NAME))) %>% distinct()

match_rate <- mean(gi$key %in% panel_keys$key)
message("Genuine time-loss injury reports (regular season): ", nrow(gi))
message("Injury-report -> panel match rate: ", round(100 * match_rate, 1), "%")

# -*- coding: utf-8 -*-
"""
NBA Player Data Scraper
=======================
This pulls the four player-side tables the R layer joins onto the schedule to
build my analysis panel. Each is saved separately for R to clean and join:

  PlayerGameLogs    -> one row per player per game appearance (my panel skeleton)
  CommonTeamRoster  -> who was on which team each season
  CommonPlayerInfo  -> birthdate, position, height/weight, draft (static bio)
  PlayerCareerStats -> a pre-season snapshot of prior-season + career totals

All the feature engineering (rolling windows, ACWR, road trips, career games
missed) happens later in R — this script only fetches and saves.

Two deliberate choices:
  * Game logs include PRE-SEASON appearances from 2021-22 on, tagged
    IS_PRESEASON = 1, so R can build a camp-participation proxy then filter them
    out. Pre-season IDs start "001", regular "002".
  * Career stats are filtered to seasons strictly BEFORE the target season, so
    the covariate is properly lagged (what did this player look like going in?).

Output : nba_player_travel/game_logs_{season}.csv
         nba_player_travel/rosters_{season}.csv
         nba_player_travel/player_bio.csv
         nba_player_travel/player_career_stats_{season}.csv

Incremental: game logs / rosters skip existing seasons (current always refreshed);
bio and career stats only fetch players not already on file.

Requires: pip install nba_api pandas
"""

import time
import random
import logging
from datetime import datetime
from pathlib import Path

import pandas as pd
from nba_api.stats.endpoints import (
    playergamelogs,
    playercareerstats,
    commonplayerinfo,
    commonteamroster,
)

# --- Configuration -----------------------------------------------------------

FIRST_SEASON           = "2015-16"
PRESEASON_FIRST_SEASON = "2021-22"   # only fetch pre-season logs from here on

ROOT_DIR   = Path(__file__).resolve().parent.parent   # repo root (parent of python/)
OUTPUT_DIR = ROOT_DIR / "data" / "raw" / "nba_player_travel"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

DELAY_MIN, DELAY_MAX = 1.0, 2.0
INTER_SEASON_PAUSE   = 3

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s  %(levelname)-7s  %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger("nba_player_travel")

# Team abbreviation -> team ID (stable across seasons). The 30 IDs and the
# reverse lookup are both derived from this, so there's one source of truth.
TEAM_ID_MAP = {
    "ATL": 1610612737, "BOS": 1610612738, "CLE": 1610612739, "NOP": 1610612740,
    "CHI": 1610612741, "DAL": 1610612742, "DEN": 1610612743, "GSW": 1610612744,
    "HOU": 1610612745, "LAC": 1610612746, "LAL": 1610612747, "MIA": 1610612748,
    "MIL": 1610612749, "MIN": 1610612750, "BKN": 1610612751, "NYK": 1610612752,
    "ORL": 1610612753, "IND": 1610612754, "PHI": 1610612755, "PHX": 1610612756,
    "POR": 1610612757, "SAC": 1610612758, "SAS": 1610612759, "OKC": 1610612760,
    "TOR": 1610612761, "UTA": 1610612762, "MEM": 1610612763, "WAS": 1610612764,
    "DET": 1610612765, "CHA": 1610612766,
}
TEAM_ID_TO_ABBR = {v: k for k, v in TEAM_ID_MAP.items()}

# Rank and fantasy columns I don't use — dropped from the game logs on the way in.
GAMELOG_DROP_COLS = [
    "NICKNAME", "NBA_FANTASY_PTS", "WNBA_FANTASY_PTS", "DD2", "TD3",
    "GP_RANK", "AVAILABLE_FLAG", "MIN_SEC", "TEAM_COUNT",
]

# Static bio fields I keep (everything R's clean step reads, nothing more).
BIO_COLS = [
    "DISPLAY_FIRST_LAST", "BIRTHDATE", "SCHOOL", "COUNTRY", "HEIGHT", "WEIGHT",
    "SEASON_EXP", "POSITION", "DRAFT_YEAR", "DRAFT_ROUND", "DRAFT_NUMBER",
]


# --- Season helpers ----------------------------------------------------------

def current_nba_season() -> str:
    """The season in progress today (the NBA season starts in October)."""
    today = datetime.today()
    start = today.year if today.month >= 10 else today.year - 1
    return f"{start}-{str(start + 1)[-2:]}"


def build_season_list(first: str, last: str) -> list[str]:
    """All season strings from first to last inclusive."""
    seasons, year = [], int(first.split("-")[0])
    while year <= int(last.split("-")[0]):
        seasons.append(f"{year}-{str(year + 1)[-2:]}")
        year += 1
    return seasons


def start_year(season: str) -> int:
    return int(season.split("-")[0])


def missing_seasons(all_seasons: list[str], prefix: str) -> list[str]:
    """Seasons with no {prefix}_{season}.csv yet; the current season is always refreshed."""
    current = current_nba_season()
    return [s for s in all_seasons
            if not (OUTPUT_DIR / f"{prefix}_{s}.csv").exists() or s == current]


def polite_sleep(min_s: float = DELAY_MIN, max_s: float = DELAY_MAX):
    time.sleep(random.uniform(min_s, max_s))


def safe_fetch(fn, label: str, retries: int = 3, result_set: int = 0, **kwargs) -> "pd.DataFrame | None":
    """Call an nba_api endpoint, return the given result set; retry with back-off, else None."""
    for attempt in range(1, retries + 1):
        try:
            return fn(**kwargs).get_data_frames()[result_set]
        except Exception as exc:
            log.warning("%s attempt %d/%d failed: %s", label, attempt, retries, exc)
            if attempt < retries:
                time.sleep(random.uniform(5 * attempt, 10 * attempt))
    log.error("%s — all retries exhausted.", label)
    return None


# --- 1. Player game logs -----------------------------------------------------

def fetch_game_logs(season: str, season_type: str) -> "pd.DataFrame | None":
    """One row per player per appearance for a season + season type."""
    df = safe_fetch(
        playergamelogs.PlayerGameLogs,
        label=f"[{season}] GameLogs ({season_type})",
        season_nullable=season,
        season_type_nullable=season_type,
        league_id_nullable="00",
    )
    if df is None or df.empty:
        return None

    df = df.drop(columns=[c for c in df.columns if c.endswith("_RANK")]
                         + [c for c in GAMELOG_DROP_COLS if c in df.columns])
    df["GAME_DATE"] = pd.to_datetime(df["GAME_DATE"], errors="coerce")
    df["GAME_ID"]   = df["GAME_ID"].astype(str)
    df["PLAYER_ID"] = df["PLAYER_ID"].astype(int)
    df["TEAM_ID"]   = df["TEAM_ID"].astype(int)

    # Home/away + opponent from the MATCHUP text ("BOS vs. NYK" / "BOS @ NYK").
    df["IS_HOME"] = df["MATCHUP"].apply(lambda x: 0 if "@" in str(x) else 1)
    df["OPPONENT_TRICODE"] = df["MATCHUP"].apply(
        lambda x: str(x).split("@")[-1].strip() if "@" in str(x)
        else str(x).split("vs.")[-1].strip()
    )
    df["IS_PRESEASON"] = int(season_type == "Pre Season")
    return df


def process_game_logs(season: str) -> None:
    """Fetch a season's regular (+ pre-season where relevant) logs and save them stacked."""
    reg = fetch_game_logs(season, "Regular Season")
    polite_sleep()

    pre = None
    if start_year(season) >= start_year(PRESEASON_FIRST_SEASON):
        pre = fetch_game_logs(season, "Pre Season")
        polite_sleep()

    frames = [d for d in (reg, pre) if d is not None]
    if not frames:
        log.warning("[%s] No game log data.", season)
        return
    df = pd.concat(frames, ignore_index=True)
    df.to_csv(OUTPUT_DIR / f"game_logs_{season}.csv", index=False)
    log.info("[%s] Game logs: %d rows (%d regular, %d pre-season)", season,
             len(df), 0 if reg is None else len(reg), 0 if pre is None else len(pre))


# --- 2. Team rosters ---------------------------------------------------------

def process_rosters(season: str) -> None:
    """Fetch all 30 rosters for a season: PLAYER_ID, PLAYER_NAME, TEAM_ID, TEAM_ABBREVIATION."""
    frames = []
    for team_id in TEAM_ID_MAP.values():
        polite_sleep(0.5, 1.2)
        df = safe_fetch(commonteamroster.CommonTeamRoster,
                        label=f"Roster {team_id}", team_id=team_id, season=season)
        if df is None or df.empty:
            continue
        name_col = next((c for c in ("PLAYER", "PLAYER_NAME") if c in df.columns), None)
        if name_col and "PLAYER_ID" in df.columns:
            row = df[["PLAYER_ID", name_col]].rename(columns={name_col: "PLAYER_NAME"}).copy()
            row["TEAM_ID"] = team_id
            row["TEAM_ABBREVIATION"] = TEAM_ID_TO_ABBR[team_id]
            frames.append(row)

    if not frames:
        log.warning("[%s] No roster data.", season)
        return
    rosters = pd.concat(frames, ignore_index=True)
    rosters["PLAYER_ID"] = rosters["PLAYER_ID"].astype(int)
    rosters["SEASON"]    = season
    rosters = rosters[["SEASON", "PLAYER_ID", "PLAYER_NAME", "TEAM_ID", "TEAM_ABBREVIATION"]].drop_duplicates()
    rosters.to_csv(OUTPUT_DIR / f"rosters_{season}.csv", index=False)
    log.info("[%s] Rosters: %d rows", season, len(rosters))


# --- 3. Static player bio (fetched once per player) --------------------------

def fetch_bio(player_id: int) -> dict:
    """Birthdate, position, height/weight and draft info for one player."""
    df = safe_fetch(commonplayerinfo.CommonPlayerInfo, label=f"Bio {player_id}", player_id=player_id)
    if df is None or df.empty:
        return {"PLAYER_ID": player_id}
    row = {c: df[c].iloc[0] for c in BIO_COLS if c in df.columns}
    row["PLAYER_ID"] = player_id
    return row


def process_bio(all_player_ids: list[int]) -> None:
    """Fetch bio only for players not already in player_bio.csv, then append."""
    path = OUTPUT_DIR / "player_bio.csv"
    have = set(pd.read_csv(path, usecols=["PLAYER_ID"])["PLAYER_ID"].astype(int)) if path.exists() else set()
    new_ids = [pid for pid in all_player_ids if pid not in have]
    log.info("Bio: %d players total | %d already on file | %d new", len(all_player_ids), len(have), len(new_ids))
    if not new_ids:
        return

    records = []
    for i, pid in enumerate(new_ids, start=1):
        polite_sleep(0.8, 1.5)
        records.append(fetch_bio(pid))
        if i % 25 == 0 or i == len(new_ids):
            log.info("  Bio: %d/%d", i, len(new_ids))

    bio = pd.DataFrame(records)
    if path.exists():
        bio = pd.concat([pd.read_csv(path), bio], ignore_index=True).drop_duplicates(subset=["PLAYER_ID"])
    bio.to_csv(path, index=False)
    log.info("Bio saved -> %d players", len(bio))


# --- 4. Career stats (a lagged pre-season snapshot per season) ---------------

def fetch_career(player_id: int, up_to_season: str) -> "pd.DataFrame | None":
    """Per-season career rows for a player, kept only for seasons BEFORE up_to_season."""
    df = safe_fetch(playercareerstats.PlayerCareerStats,
                    label=f"Career {player_id}", player_id=player_id, per_mode36="PerGame")
    if df is None or df.empty:
        return None
    df["PLAYER_ID"] = player_id
    df = df[df["SEASON_ID"].apply(lambda s: int(str(s).split("-")[0]) if "-" in str(s) else 0)
            < start_year(up_to_season)]
    if df.empty:
        return None  # rookie — no prior career yet
    df["SNAPSHOT_SEASON"] = up_to_season
    return df


def process_career(season: str, player_ids: list[int]) -> None:
    """
    Save one career snapshot file per season. Past seasons are fixed once written,
    so we skip them; the current season is topped up with any new players only
    (the "career up to the start of this season" snapshot never changes for a
    player already on file).
    """
    path = OUTPUT_DIR / f"player_career_stats_{season}.csv"
    is_current = season == current_nba_season()

    if path.exists() and not is_current:
        log.info("[%s] Career snapshot already exists — skipping.", season)
        return

    existing = None
    if path.exists() and is_current:
        existing = pd.read_csv(path, low_memory=False)
        have = set(existing["PLAYER_ID"].astype(int))
        player_ids = [pid for pid in player_ids if pid not in have]
        if not player_ids:
            log.info("[%s] Career snapshot up to date.", season)
            return

    log.info("[%s] Fetching career snapshot for %d players ...", season, len(player_ids))
    frames = []
    for i, pid in enumerate(player_ids, start=1):
        polite_sleep(0.8, 1.5)
        career = fetch_career(pid, up_to_season=season)
        if career is not None:
            frames.append(career)
        if i % 25 == 0 or i == len(player_ids):
            log.info("  [%s] Career: %d/%d", season, i, len(player_ids))

    if not frames:
        log.info("[%s] No career rows (all rookies?).", season)
        return
    career_df = pd.concat(frames, ignore_index=True)
    if existing is not None:
        career_df = pd.concat([existing, career_df], ignore_index=True) \
            .drop_duplicates(subset=["PLAYER_ID", "SEASON_ID"])
    career_df.to_csv(path, index=False)
    log.info("[%s] Career saved -> %d rows, %d players", season, len(career_df), career_df["PLAYER_ID"].nunique())


def regular_season_player_ids(season: str) -> list[int]:
    """Regular-season player IDs from a saved game-log file (drives bio + career fetches)."""
    path = OUTPUT_DIR / f"game_logs_{season}.csv"
    if not path.exists():
        return []
    cols = pd.read_csv(path, nrows=0).columns
    use = ["PLAYER_ID"] + (["IS_PRESEASON"] if "IS_PRESEASON" in cols else [])
    df = pd.read_csv(path, usecols=use, low_memory=False)
    if "IS_PRESEASON" in df.columns:
        df = df[df["IS_PRESEASON"] == 0]
    return df["PLAYER_ID"].dropna().astype(int).unique().tolist()


# --- Main --------------------------------------------------------------------

def main() -> None:
    all_seasons = build_season_list(FIRST_SEASON, current_nba_season())
    log.info("Season range: %s -> %s | pre-season logs from %s",
             all_seasons[0], all_seasons[-1], PRESEASON_FIRST_SEASON)

    # Step 1 — game logs
    log.info("=" * 55 + "\nSTEP 1: Game logs")
    for season in missing_seasons(all_seasons, "game_logs"):
        process_game_logs(season)
        time.sleep(INTER_SEASON_PAUSE)

    # Step 2 — rosters
    log.info("=" * 55 + "\nSTEP 2: Rosters")
    for season in missing_seasons(all_seasons, "rosters"):
        process_rosters(season)
        time.sleep(INTER_SEASON_PAUSE)

    # Step 3 — bio (every player who ever appears in a saved game log)
    log.info("=" * 55 + "\nSTEP 3: Player bio")
    every_player = sorted({pid for s in all_seasons for pid in regular_season_player_ids(s)})
    process_bio(every_player)

    # Step 4 — career snapshots
    log.info("=" * 55 + "\nSTEP 4: Career stats")
    for season in all_seasons:
        ids = regular_season_player_ids(season)
        if ids:
            process_career(season, ids)
            time.sleep(INTER_SEASON_PAUSE)

    log.info("Done. Output in %s", OUTPUT_DIR.resolve())


if __name__ == "__main__":
    main()

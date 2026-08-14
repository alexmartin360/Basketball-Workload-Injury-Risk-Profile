# -*- coding: utf-8 -*-
"""
NBA Player Data Scraper
Pulls the player-side tables the R layer joins onto the schedule: game logs (the panel
skeleton), static bio, and a per-season career snapshot. Game logs include pre-season
appearances from 2021-22 on, tagged IS_PRESEASON, which is the training-camp proxy.
Career rows are filtered to seasons strictly BEFORE the target season, so the covariate
is properly lagged.
Output: data/raw/nba_player_travel/{game_logs_{season}, player_bio, player_career_stats_{season}}.csv
"""

import time
import random
import logging
from datetime import datetime
from pathlib import Path

import pandas as pd
from nba_api.stats.endpoints import playergamelogs, playercareerstats, commonplayerinfo

FIRST_SEASON           = "2015-16"
PRESEASON_FIRST_SEASON = "2021-22"

ROOT_DIR   = Path(__file__).resolve().parent.parent
OUTPUT_DIR = ROOT_DIR / "data" / "raw" / "nba_player_travel"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

logging.basicConfig(level=logging.INFO, format="%(asctime)s  %(levelname)-7s  %(message)s",
                    datefmt="%H:%M:%S")
log = logging.getLogger("nba_player_travel")

# Rank and fantasy columns nothing downstream uses.
GAMELOG_DROP_COLS = ["NICKNAME", "NBA_FANTASY_PTS", "WNBA_FANTASY_PTS", "DD2", "TD3",
                     "GP_RANK", "AVAILABLE_FLAG", "MIN_SEC", "TEAM_COUNT"]

BIO_COLS = ["DISPLAY_FIRST_LAST", "BIRTHDATE", "SCHOOL", "COUNTRY", "HEIGHT", "WEIGHT",
            "SEASON_EXP", "POSITION", "DRAFT_YEAR", "DRAFT_ROUND", "DRAFT_NUMBER"]


def current_nba_season() -> str:
    today = datetime.today()
    start = today.year if today.month >= 10 else today.year - 1
    return f"{start}-{str(start + 1)[-2:]}"


def build_season_list(first: str, last: str) -> list[str]:
    years = range(int(first.split("-")[0]), int(last.split("-")[0]) + 1)
    return [f"{y}-{str(y + 1)[-2:]}" for y in years]


def start_year(season: str) -> int:
    return int(season.split("-")[0])


def polite_sleep(min_s: float = 1.0, max_s: float = 2.0):
    time.sleep(random.uniform(min_s, max_s))


def safe_fetch(fn, label: str, retries: int = 3, **kwargs) -> "pd.DataFrame | None":
    """Call an nba_api endpoint and return result set 0; retry with back-off, else None."""
    for attempt in range(1, retries + 1):
        try:
            return fn(**kwargs).get_data_frames()[0]
        except Exception as exc:
            log.warning("%s attempt %d/%d failed: %s", label, attempt, retries, exc)
            if attempt < retries:
                time.sleep(random.uniform(5 * attempt, 10 * attempt))
    log.error("%s - all retries exhausted.", label)
    return None


# --- Game logs ---------------------------------------------------------------

def fetch_game_logs(season: str, season_type: str) -> "pd.DataFrame | None":
    df = safe_fetch(playergamelogs.PlayerGameLogs, f"[{season}] GameLogs ({season_type})",
                    season_nullable=season, season_type_nullable=season_type,
                    league_id_nullable="00")
    if df is None or df.empty:
        return None

    df = df.drop(columns=[c for c in df.columns if c.endswith("_RANK")]
                         + [c for c in GAMELOG_DROP_COLS if c in df.columns])
    df["GAME_DATE"] = pd.to_datetime(df["GAME_DATE"], errors="coerce")
    df["GAME_ID"]   = df["GAME_ID"].astype(str)
    df["PLAYER_ID"] = df["PLAYER_ID"].astype(int)
    df["TEAM_ID"]   = df["TEAM_ID"].astype(int)

    # Home/away and opponent come out of the MATCHUP text ("BOS vs. NYK" / "BOS @ NYK").
    away = df["MATCHUP"].astype(str).str.contains("@")
    df["IS_HOME"] = (~away).astype(int)
    df["OPPONENT_TRICODE"] = (df["MATCHUP"].astype(str)
                                .str.split("@|vs\\.", regex=True).str[-1].str.strip())
    df["IS_PRESEASON"] = int(season_type == "Pre Season")
    return df


def process_game_logs(season: str) -> None:
    frames = [fetch_game_logs(season, "Regular Season")]
    polite_sleep()
    if start_year(season) >= start_year(PRESEASON_FIRST_SEASON):
        frames.append(fetch_game_logs(season, "Pre Season"))
        polite_sleep()

    frames = [d for d in frames if d is not None]
    if not frames:
        log.warning("[%s] No game log data.", season)
        return
    df = pd.concat(frames, ignore_index=True)
    df.to_csv(OUTPUT_DIR / f"game_logs_{season}.csv", index=False)
    log.info("[%s] Game logs: %d rows", season, len(df))


# --- Bio and career ----------------------------------------------------------

def process_bio(all_player_ids: list[int]) -> None:
    """Fetch bio only for players not already on file, then append."""
    path = OUTPUT_DIR / "player_bio.csv"
    have = set(pd.read_csv(path, usecols=["PLAYER_ID"])["PLAYER_ID"].astype(int)) if path.exists() else set()
    new_ids = [pid for pid in all_player_ids if pid not in have]
    log.info("Bio: %d players | %d on file | %d new", len(all_player_ids), len(have), len(new_ids))
    if not new_ids:
        return

    records = []
    for i, pid in enumerate(new_ids, start=1):
        polite_sleep(0.8, 1.5)
        df = safe_fetch(commonplayerinfo.CommonPlayerInfo, f"Bio {pid}", player_id=pid)
        row = {c: df[c].iloc[0] for c in BIO_COLS if c in df.columns} if df is not None and not df.empty else {}
        records.append({**row, "PLAYER_ID": pid})
        if i % 25 == 0 or i == len(new_ids):
            log.info("  Bio: %d/%d", i, len(new_ids))

    bio = pd.DataFrame(records)
    if path.exists():
        bio = pd.concat([pd.read_csv(path), bio], ignore_index=True).drop_duplicates(subset=["PLAYER_ID"])
    bio.to_csv(path, index=False)
    log.info("Bio saved -> %d players", len(bio))


def process_career(season: str, player_ids: list[int]) -> None:
    """
    One career snapshot per season. Past snapshots never change once written, so they
    are skipped; the current season is topped up with new players only.
    """
    path = OUTPUT_DIR / f"player_career_stats_{season}.csv"
    is_current = season == current_nba_season()
    if path.exists() and not is_current:
        return

    existing = None
    if path.exists():
        existing = pd.read_csv(path, low_memory=False)
        have = set(existing["PLAYER_ID"].astype(int))
        player_ids = [pid for pid in player_ids if pid not in have]
        if not player_ids:
            log.info("[%s] Career snapshot up to date.", season)
            return

    log.info("[%s] Fetching career for %d players ...", season, len(player_ids))
    frames = []
    for i, pid in enumerate(player_ids, start=1):
        polite_sleep(0.8, 1.5)
        df = safe_fetch(playercareerstats.PlayerCareerStats, f"Career {pid}",
                        player_id=pid, per_mode36="PerGame")
        if df is None or df.empty:
            continue
        # Seasons strictly before the target one, so the covariate is lagged.
        df = df[df["SEASON_ID"].str.split("-").str[0].astype(int) < start_year(season)]
        if not df.empty:
            frames.append(df.assign(PLAYER_ID=pid, SNAPSHOT_SEASON=season))
        if i % 25 == 0 or i == len(player_ids):
            log.info("  [%s] Career: %d/%d", season, i, len(player_ids))

    if not frames:
        return
    career = pd.concat(frames, ignore_index=True)
    if existing is not None:
        career = pd.concat([existing, career], ignore_index=True) \
                   .drop_duplicates(subset=["PLAYER_ID", "SEASON_ID"])
    career.to_csv(path, index=False)
    log.info("[%s] Career saved -> %d rows", season, len(career))


def regular_season_player_ids(season: str) -> list[int]:
    """Regular-season player IDs from a saved game-log file."""
    path = OUTPUT_DIR / f"game_logs_{season}.csv"
    if not path.exists():
        return []
    cols = pd.read_csv(path, nrows=0).columns
    use = ["PLAYER_ID"] + (["IS_PRESEASON"] if "IS_PRESEASON" in cols else [])
    df = pd.read_csv(path, usecols=use, low_memory=False)
    if "IS_PRESEASON" in df.columns:
        df = df[df["IS_PRESEASON"] == 0]
    return df["PLAYER_ID"].dropna().astype(int).unique().tolist()


def main() -> None:
    all_seasons = build_season_list(FIRST_SEASON, current_nba_season())
    current     = current_nba_season()
    log.info("Seasons %s -> %s", all_seasons[0], all_seasons[-1])

    for season in all_seasons:
        if not (OUTPUT_DIR / f"game_logs_{season}.csv").exists() or season == current:
            process_game_logs(season)
            time.sleep(3)

    process_bio(sorted({pid for s in all_seasons for pid in regular_season_player_ids(s)}))

    for season in all_seasons:
        ids = regular_season_player_ids(season)
        if ids:
            process_career(season, ids)
            time.sleep(3)

    log.info("Done. Output in %s", OUTPUT_DIR.resolve())


if __name__ == "__main__":
    main()

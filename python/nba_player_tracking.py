# -*- coding: utf-8 -*-
"""
NBA In-Game Player Load Scraper
===============================
This gives me the "external load" side of my workload measures: how far each
player actually moved on court, not just how many minutes the coach gave them.
That distinction matters in my thesis — minutes is a coaching decision, distance
is what the body absorbed.

For every regular-season game I pull two player-level box scores and merge them
into one row per player per game:
  BoxScorePlayerTrackV3 -> distance, speed, touches   (the tracking load)
  BoxScoreAdvancedV3    -> usagePercentage, possessions, pace  (game context)

I anchor on the tracking endpoint (that is where distance lives) and left-join
advanced onto it by player. I originally also collected hustle, misc and
traditional box scores, but none of those columns survive into the R panel, so
I dropped them — two endpoints per game instead of five.

It is incremental: I skip games already saved, and re-check the last 5 games of
the current season in case the league revised their stats after the fact.

Output : nba_game_load/nba_game_load_{season}.parquet   (one per season)
         nba_game_load/nba_game_load_all_seasons.parquet (merged, the R input)
Join key onto the player panel: PLAYER_ID + GAME_ID + SEASON.

Requires: pip install nba_api pandas pyarrow
"""

import time
import random
import logging
from datetime import datetime
from pathlib import Path

import pandas as pd
from nba_api.stats.endpoints import (
    boxscoreplayertrackv3,
    boxscoreadvancedv3,
    leaguegamefinder,
)

# --- Configuration -----------------------------------------------------------

FIRST_SEASON = "2016-17"
ROOT_DIR     = Path(__file__).resolve().parent.parent   # repo root (parent of python/)
OUTPUT_DIR   = ROOT_DIR / "data" / "raw" / "nba_game_load"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

DELAY_MIN, DELAY_MAX = 0.5, 1.0    # polite pause between calls
BATCH_SIZE, BATCH_PAUSE = 100, 5   # longer breather every 100 games

# The only columns I keep. personId/gameId are the join keys; the rest are the
# load + context measures the R panel actually uses.
TRACK_COLS    = ["gameId", "personId", "distance", "speed", "touches"]
ADVANCED_COLS = ["gameId", "personId", "usagePercentage", "possessions", "pace"]

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s  %(levelname)-7s  %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger("nba_game_load")


# --- Season helpers ----------------------------------------------------------

def current_nba_season() -> str:
    """The season in progress today (the NBA season starts in October)."""
    today = datetime.today()
    start = today.year if today.month >= 10 else today.year - 1
    return f"{start}-{str(start + 1)[-2:]}"


def build_season_list(first: str, last: str) -> list[str]:
    """All season strings from first to last inclusive, e.g. '2016-17' ... '2025-26'."""
    seasons, year = [], int(first.split("-")[0])
    while year <= int(last.split("-")[0]):
        seasons.append(f"{year}-{str(year + 1)[-2:]}")
        year += 1
    return seasons


def fetched_game_ids(season: str) -> set[str]:
    """Game IDs already on disk for a season — the basis for skipping re-fetches."""
    path = OUTPUT_DIR / f"nba_game_load_{season}.parquet"
    if not path.exists():
        return set()
    return set(pd.read_parquet(path, columns=["GAME_ID"])["GAME_ID"].astype(str))


# --- API helpers -------------------------------------------------------------

def polite_sleep():
    time.sleep(random.uniform(DELAY_MIN, DELAY_MAX))


def safe_fetch(endpoint_cls, label: str, retries: int = 3, **kwargs) -> "pd.DataFrame | None":
    """Call an endpoint and return player result set 0; retry with back-off, else None."""
    for attempt in range(1, retries + 1):
        try:
            df = endpoint_cls(**kwargs).get_data_frames()[0]
            return df if (df is not None and not df.empty) else None
        except Exception as exc:
            log.debug("%s attempt %d/%d failed: %s", label, attempt, retries, exc)
            if attempt < retries:
                time.sleep(random.uniform(5 * attempt, 12 * attempt))
    return None


def season_game_ids(season: str) -> list[str]:
    """Every regular-season game ID for a season (sorted, unique)."""
    df = safe_fetch(
        leaguegamefinder.LeagueGameFinder,
        label=f"[{season}] GameFinder",
        season_nullable=season,
        season_type_nullable="Regular Season",
        league_id_nullable="00",
    )
    if df is None:
        log.warning("[%s] No game IDs returned.", season)
        return []
    return sorted(df["GAME_ID"].astype(str).unique())


# --- Per-game assembly -------------------------------------------------------

def fetch_game_load(game_id: str) -> "pd.DataFrame | None":
    """
    One row per player for a game: tracking (distance/speed/touches) with the
    advanced context columns left-joined on. Tracking is the anchor — if it
    fails I skip the game, since distance is the whole point.
    """
    polite_sleep()
    track = safe_fetch(boxscoreplayertrackv3.BoxScorePlayerTrackV3,
                       label=f"Tracking {game_id}", game_id=game_id)
    if track is None:
        return None
    track = track[[c for c in TRACK_COLS if c in track.columns]].copy()

    polite_sleep()
    adv = safe_fetch(boxscoreadvancedv3.BoxScoreAdvancedV3,
                     label=f"Advanced {game_id}", game_id=game_id)
    if adv is not None:
        adv = adv[[c for c in ADVANCED_COLS if c in adv.columns]]
        track = track.merge(adv.drop(columns=["gameId"]), on="personId", how="left")

    # Match the naming the other scrapers use so R can join cleanly.
    track.columns = [c.upper() for c in track.columns]
    return track.rename(columns={"PERSONID": "PLAYER_ID", "GAMEID": "GAME_ID"})


# --- Season processing -------------------------------------------------------

def process_season(season: str) -> int:
    """Fetch every not-yet-saved game for a season, append to its Parquet. Returns games added."""
    time.sleep(random.uniform(3, 5))
    all_ids = season_game_ids(season)
    if not all_ids:
        return 0

    have = fetched_game_ids(season)
    # Current season: also re-fetch the last 5 games (the league revises stats post-game).
    if season == current_nba_season() and have:
        recent = set(all_ids[-5:])
        todo = [g for g in all_ids if g not in have or g in recent]
    else:
        todo = [g for g in all_ids if g not in have]

    log.info("[%s] %d games | %d on disk | %d to fetch", season, len(all_ids), len(have), len(todo))
    if not todo:
        return 0

    rows: list[pd.DataFrame] = []
    for i, game_id in enumerate(todo, start=1):
        df = fetch_game_load(game_id)
        if df is not None:
            df["SEASON"] = season
            rows.append(df)
        if i % 50 == 0 or i == len(todo):
            log.info("[%s] %d/%d fetched", season, i, len(todo))
        if i % BATCH_SIZE == 0 and i < len(todo):
            time.sleep(BATCH_PAUSE)

    if not rows:
        log.warning("[%s] No new data collected.", season)
        return 0

    new_df = pd.concat(rows, ignore_index=True)

    # Merge with what's already saved, replacing any games we re-fetched.
    path = OUTPUT_DIR / f"nba_game_load_{season}.parquet"
    if path.exists() and have:
        existing = pd.read_parquet(path)
        existing = existing[~existing["GAME_ID"].astype(str).isin(new_df["GAME_ID"].astype(str))]
        new_df = pd.concat([existing, new_df], ignore_index=True)

    new_df = new_df.sort_values(["GAME_ID", "PLAYER_ID"]).reset_index(drop=True)
    new_df.to_parquet(path, index=False)
    log.info("[%s] Saved %d rows -> %s", season, len(new_df), path)
    return len(rows)


def consolidate(seasons: list[str]) -> None:
    """Merge all season Parquets into the single combined file R reads."""
    frames = [pd.read_parquet(OUTPUT_DIR / f"nba_game_load_{s}.parquet")
              for s in seasons if (OUTPUT_DIR / f"nba_game_load_{s}.parquet").exists()]
    if not frames:
        log.error("No season files found — combined file not written.")
        return
    combined = pd.concat(frames, ignore_index=True)
    out = OUTPUT_DIR / "nba_game_load_all_seasons.parquet"
    combined.to_parquet(out, index=False)
    log.info("Combined -> %s (%d rows, %d seasons)", out, len(combined), len(frames))


# --- Main --------------------------------------------------------------------

def main() -> None:
    seasons = build_season_list(FIRST_SEASON, current_nba_season())
    log.info("Seasons: %s -> %s", seasons[0], seasons[-1])

    any_new = False
    for i, season in enumerate(seasons):
        log.info("=" * 55)
        log.info("Season %s  (%d/%d)", season, i + 1, len(seasons))
        if process_season(season) > 0:
            any_new = True
        if i < len(seasons) - 1:
            time.sleep(2)

    if any_new:
        consolidate(seasons)
    else:
        log.info("No new games — combined file left as is "
                 "(delete it and re-run to force a rebuild).")


if __name__ == "__main__":
    main()

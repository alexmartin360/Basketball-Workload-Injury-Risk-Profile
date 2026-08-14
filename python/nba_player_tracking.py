# -*- coding: utf-8 -*-
"""
NBA In-Game Player Load Scraper
The external-load side of the workload measures: how far each player actually moved on
court, rather than how many minutes the coach gave them. Per game it merges tracking
(distance, speed, touches) with advanced context (usage, possessions, pace) into one row
per player. Tracking is the anchor - a game without it is skipped, since distance is the
whole point.
Output: data/raw/nba_game_load/nba_game_load_{season}.parquet (+ a combined file).
Join key onto the panel: PLAYER_ID + GAME_ID + SEASON.
"""

import time
import random
import logging
from datetime import datetime
from pathlib import Path

import pandas as pd
from nba_api.stats.endpoints import (boxscoreplayertrackv3, boxscoreadvancedv3,
                                     leaguegamefinder)

FIRST_SEASON = "2016-17"
ROOT_DIR     = Path(__file__).resolve().parent.parent
OUTPUT_DIR   = ROOT_DIR / "data" / "raw" / "nba_game_load"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

TRACK_COLS    = ["gameId", "personId", "distance", "speed", "touches"]
ADVANCED_COLS = ["gameId", "personId", "usagePercentage", "possessions", "pace"]

logging.basicConfig(level=logging.INFO, format="%(asctime)s  %(levelname)-7s  %(message)s",
                    datefmt="%H:%M:%S")
log = logging.getLogger("nba_game_load")


def current_nba_season() -> str:
    today = datetime.today()
    start = today.year if today.month >= 10 else today.year - 1
    return f"{start}-{str(start + 1)[-2:]}"


def build_season_list(first: str, last: str) -> list[str]:
    years = range(int(first.split("-")[0]), int(last.split("-")[0]) + 1)
    return [f"{y}-{str(y + 1)[-2:]}" for y in years]


def season_path(season: str) -> Path:
    return OUTPUT_DIR / f"nba_game_load_{season}.parquet"


def safe_fetch(endpoint_cls, label: str, retries: int = 3, **kwargs) -> "pd.DataFrame | None":
    """Call an endpoint and return the player result set; retry with back-off, else None."""
    for attempt in range(1, retries + 1):
        try:
            df = endpoint_cls(**kwargs).get_data_frames()[0]
            return df if (df is not None and not df.empty) else None
        except Exception as exc:
            log.debug("%s attempt %d/%d failed: %s", label, attempt, retries, exc)
            if attempt < retries:
                time.sleep(random.uniform(5 * attempt, 12 * attempt))
    return None


def fetch_game_load(game_id: str) -> "pd.DataFrame | None":
    """One row per player for a game: tracking, with advanced context joined on."""
    time.sleep(random.uniform(0.5, 1.0))
    track = safe_fetch(boxscoreplayertrackv3.BoxScorePlayerTrackV3,
                       f"Tracking {game_id}", game_id=game_id)
    if track is None:
        return None
    track = track[[c for c in TRACK_COLS if c in track.columns]].copy()

    time.sleep(random.uniform(0.5, 1.0))
    adv = safe_fetch(boxscoreadvancedv3.BoxScoreAdvancedV3, f"Advanced {game_id}", game_id=game_id)
    if adv is not None:
        adv = adv[[c for c in ADVANCED_COLS if c in adv.columns]]
        track = track.merge(adv.drop(columns=["gameId"]), on="personId", how="left")

    track.columns = [c.upper() for c in track.columns]
    return track.rename(columns={"PERSONID": "PLAYER_ID", "GAMEID": "GAME_ID"})


def process_season(season: str) -> int:
    """Fetch every not-yet-saved game for a season and append. Returns games added."""
    time.sleep(random.uniform(3, 5))
    df = safe_fetch(leaguegamefinder.LeagueGameFinder, f"[{season}] GameFinder",
                    season_nullable=season, season_type_nullable="Regular Season",
                    league_id_nullable="00")
    if df is None:
        log.warning("[%s] No game IDs returned.", season)
        return 0
    all_ids = sorted(df["GAME_ID"].astype(str).unique())

    path = season_path(season)
    have = (set(pd.read_parquet(path, columns=["GAME_ID"])["GAME_ID"].astype(str))
            if path.exists() else set())
    # The league revises stats after the fact, so the current season re-fetches its last 5.
    recent = set(all_ids[-5:]) if (season == current_nba_season() and have) else set()
    todo = [g for g in all_ids if g not in have or g in recent]

    log.info("[%s] %d games | %d on disk | %d to fetch", season, len(all_ids), len(have), len(todo))
    if not todo:
        return 0

    rows = []
    for i, game_id in enumerate(todo, start=1):
        game = fetch_game_load(game_id)
        if game is not None:
            rows.append(game.assign(SEASON=season))
        if i % 50 == 0 or i == len(todo):
            log.info("[%s] %d/%d fetched", season, i, len(todo))
        if i % 100 == 0 and i < len(todo):
            time.sleep(5)

    if not rows:
        log.warning("[%s] No new data collected.", season)
        return 0

    new_df = pd.concat(rows, ignore_index=True)
    if have:
        existing = pd.read_parquet(path)
        keep = ~existing["GAME_ID"].astype(str).isin(new_df["GAME_ID"].astype(str))
        new_df = pd.concat([existing[keep], new_df], ignore_index=True)

    new_df.sort_values(["GAME_ID", "PLAYER_ID"]).reset_index(drop=True).to_parquet(path, index=False)
    log.info("[%s] Saved %d rows", season, len(new_df))
    return len(rows)


def main() -> None:
    seasons = build_season_list(FIRST_SEASON, current_nba_season())
    log.info("Seasons: %s -> %s", seasons[0], seasons[-1])

    # Listed, not a generator: every season must run, not just up to the first hit.
    added = [process_season(season) for season in seasons]

    if not any(n > 0 for n in added):
        log.info("No new games - combined file left as is.")
        return
    frames = [pd.read_parquet(season_path(s)) for s in seasons if season_path(s).exists()]
    if frames:
        pd.concat(frames, ignore_index=True).to_parquet(
            OUTPUT_DIR / "nba_game_load_all_seasons.parquet", index=False)
        log.info("Combined -> %d rows, %d seasons", sum(map(len, frames)), len(frames))


if __name__ == "__main__":
    main()

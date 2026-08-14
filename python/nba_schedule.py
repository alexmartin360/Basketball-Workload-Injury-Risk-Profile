# -*- coding: utf-8 -*-
"""
NBA Schedule & Travel Scraper
Builds the team-level schedule context: rest days, back-to-backs, distance flown and
late local tip-offs, one row per team per game.
Output: data/raw/nba_schedule/nba_schedule_{season}.csv (+ a combined file).
Incremental - existing seasons are skipped, the current one is always refreshed.
"""

import time
import random
import logging
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo

import numpy as np
import pandas as pd
from nba_api.stats.endpoints import scheduleleaguev2

FIRST_SEASON = "2015-16"
ROOT_DIR     = Path(__file__).resolve().parent.parent
OUTPUT_DIR   = ROOT_DIR / "data" / "raw" / "nba_schedule"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

logging.basicConfig(level=logging.INFO, format="%(asctime)s  %(levelname)-7s  %(message)s",
                    datefmt="%H:%M:%S")
log = logging.getLogger("nba_schedule")

# Arena city -> (lat, lon, timezone). Covers relocations and the neutral / international
# venues in the study window; a city missing here produces null travel, so the run warns.
ARENA_LOOKUP = {
    "Atlanta":        (33.7573, -84.3963,  "America/New_York"),
    "Boston":         (42.3662, -71.0621,  "America/New_York"),
    "Brooklyn":       (40.6826, -73.9754,  "America/New_York"),
    "Charlotte":      (35.2251, -80.8392,  "America/New_York"),
    "Chicago":        (41.8807, -87.6742,  "America/Chicago"),
    "Cleveland":      (41.4965, -81.6882,  "America/New_York"),
    "Dallas":         (32.7905, -96.8103,  "America/Chicago"),
    "Denver":         (39.7487, -105.0077, "America/Denver"),
    "Detroit":        (42.3410, -83.0552,  "America/New_York"),
    "Golden State":   (37.7680, -122.3877, "America/Los_Angeles"),
    "San Francisco":  (37.7680, -122.3877, "America/Los_Angeles"),
    "Houston":        (29.7508, -95.3621,  "America/Chicago"),
    "Indiana":        (39.7640, -86.1555,  "America/Indiana/Indianapolis"),
    "Indianapolis":   (39.7640, -86.1555,  "America/Indiana/Indianapolis"),
    "LA Clippers":    (34.0430, -118.2673, "America/Los_Angeles"),
    "LA Lakers":      (34.0430, -118.2673, "America/Los_Angeles"),
    "Los Angeles":    (34.0430, -118.2673, "America/Los_Angeles"),
    "Memphis":        (35.1382, -90.0505,  "America/Chicago"),
    "Miami":          (25.7814, -80.1870,  "America/New_York"),
    "Milwaukee":      (43.0451, -87.9170,  "America/Chicago"),
    "Minneapolis":    (44.9795, -93.2760,  "America/Chicago"),
    "Minnesota":      (44.9795, -93.2760,  "America/Chicago"),
    "New Orleans":    (29.9490, -90.0821,  "America/Chicago"),
    "New York":       (40.7505, -73.9934,  "America/New_York"),
    "Oklahoma City":  (35.4634, -97.5151,  "America/Chicago"),
    "Orlando":        (28.5392, -81.3839,  "America/New_York"),
    "Philadelphia":   (39.9012, -75.1720,  "America/New_York"),
    "Phoenix":        (33.4457, -112.0712, "America/Phoenix"),
    "Portland":       (45.5316, -122.6668, "America/Los_Angeles"),
    "Sacramento":     (38.5802, -121.4997, "America/Los_Angeles"),
    "San Antonio":    (29.4270, -98.4375,  "America/Chicago"),
    "Seattle":        (47.6220, -122.3540, "America/Los_Angeles"),
    "Toronto":        (43.6435, -79.3791,  "America/Toronto"),
    "Utah":           (40.7683, -111.9011, "America/Denver"),
    "Salt Lake City": (40.7683, -111.9011, "America/Denver"),
    "Washington":     (38.8981, -77.0209,  "America/New_York"),
    "Las Vegas":      (36.0909, -115.1778, "America/Los_Angeles"),
    "Inglewood":      (33.9451, -118.3431, "America/Los_Angeles"),
    "Austin":         (30.2030, -97.7220,  "America/Chicago"),
    "Paris":          (48.8939, 2.3567,    "Europe/Paris"),
    "London":         (51.5550, -0.0173,   "Europe/London"),
    "Berlin":         (52.5063, 13.4432,   "Europe/Berlin"),
    "Mexico City":    (19.4061, -99.0907,  "America/Mexico_City"),
    "Mexico City, Mexico": (19.4061, -99.0907, "America/Mexico_City"),
}


def current_nba_season() -> str:
    today = datetime.today()
    start = today.year if today.month >= 10 else today.year - 1
    return f"{start}-{str(start + 1)[-2:]}"


def build_season_list(first: str, last: str) -> list[str]:
    years = range(int(first.split("-")[0]), int(last.split("-")[0]) + 1)
    return [f"{y}-{str(y + 1)[-2:]}" for y in years]


def get_arena_info(city: str):
    """(lat, lon, tz) for an arena city, with a fuzzy substring fallback."""
    if not city:
        return None
    if city in ARENA_LOOKUP:
        return ARENA_LOOKUP[city]
    return next((v for k, v in ARENA_LOOKUP.items()
                 if k.lower() in city.lower() or city.lower() in k.lower()), None)


def haversine_km(lat1, lon1, lat2, lon2):
    """Great-circle distance in km; works on scalars or arrays."""
    lat1, lon1, lat2, lon2 = map(np.radians, (lat1, lon1, lat2, lon2))
    a = np.sin((lat2 - lat1) / 2) ** 2 + np.cos(lat1) * np.cos(lat2) * np.sin((lon2 - lon1) / 2) ** 2
    return 6371.0 * 2 * np.arctan2(np.sqrt(a), np.sqrt(1 - a))


def fetch_schedule(season: str) -> "pd.DataFrame | None":
    for attempt in range(1, 4):
        try:
            return scheduleleaguev2.ScheduleLeagueV2(season=season,
                                                     league_id="00").get_data_frames()[0]
        except Exception as exc:
            log.warning("[%s] Schedule attempt %d/3 failed: %s", season, attempt, exc)
            time.sleep(random.uniform(5 * attempt, 10 * attempt))
    log.error("[%s] Schedule - all retries exhausted.", season)
    return None


def enrich_schedule(schedule: pd.DataFrame) -> pd.DataFrame:
    """Keep the columns needed, attach arena lat/lon, and flag late local tip-offs."""
    keep = ["gameId", "gameDate", "gameDateTimeUTC", "monthNum", "weekNumber", "arenaCity",
            "homeTeam_teamId", "homeTeam_teamTricode",
            "awayTeam_teamId", "awayTeam_teamTricode"]
    df = schedule[[c for c in keep if c in schedule.columns]].copy()
    df["gameDateTimeUTC"] = pd.to_datetime(df["gameDateTimeUTC"], utc=True, errors="coerce")
    df["gameDate"] = pd.to_datetime(df["gameDate"], errors="coerce").dt.date

    info = df["arenaCity"].astype(str).map(get_arena_info)
    df["ARENA_LAT"] = info.map(lambda v: v[0] if v else None)
    df["ARENA_LON"] = info.map(lambda v: v[1] if v else None)

    # Tip-off at or after 9pm local, converted a timezone at a time.
    tz = info.map(lambda v: v[2] if v else None)
    df["IS_LATE_GAME"] = None
    for zone, rows in df.groupby(tz).groups.items():
        local = df.loc[rows, "gameDateTimeUTC"].dt.tz_convert(ZoneInfo(zone))
        df.loc[rows, "IS_LATE_GAME"] = local.dt.hour.ge(21).astype(float).where(local.notna())

    # Only regular-season venues reach the panel, so only warn about those.
    unmapped = df.loc[info.isna() & df["gameId"].astype(str).str.startswith("002"), "arenaCity"]
    if not unmapped.empty:
        log.warning("Unmapped regular-season arenaCity (add to ARENA_LOOKUP): %s",
                    ", ".join(f"{c} (x{n})" for c, n in unmapped.value_counts().items()))
    return df


def add_team_game_features(df: pd.DataFrame) -> pd.DataFrame:
    """Expand to one row per team per game and add rest, back-to-back and travel."""
    def side(is_home: int) -> pd.DataFrame:
        me, them = ("home", "away") if is_home else ("away", "home")
        out = df.copy()
        out["TEAM_ID"]          = out[f"{me}Team_teamId"]
        out["TEAM_TRICODE"]     = out[f"{me}Team_teamTricode"]
        out["OPPONENT_TRICODE"] = out[f"{them}Team_teamTricode"]
        out["IS_HOME"], out["IS_AWAY"] = is_home, 1 - is_home
        return out

    long_df = pd.concat([side(1), side(0)], ignore_index=True) \
                .sort_values(["TEAM_ID", "gameDateTimeUTC"]).reset_index(drop=True)

    # Rest is full calendar days between games, so zero rest is a back-to-back.
    game_day  = pd.to_datetime(long_df["gameDate"])
    prev_date = game_day.groupby(long_df["TEAM_ID"]).shift(1)
    long_df["DAYS_REST"] = (game_day - prev_date).dt.days - 1
    long_df["IS_BACK_TO_BACK"] = (long_df["DAYS_REST"] == 0).astype("Int64")
    long_df.loc[prev_date.isna(), ["DAYS_REST", "IS_BACK_TO_BACK"]] = None

    # Travel is the distance flown from the previous arena, counted on away games only.
    lat, lon = long_df["ARENA_LAT"].astype(float), long_df["ARENA_LON"].astype(float)
    prev_lat = lat.groupby(long_df["TEAM_ID"]).shift(1)
    prev_lon = lon.groupby(long_df["TEAM_ID"]).shift(1)
    km = pd.Series(haversine_km(prev_lat, prev_lon, lat, lon), index=long_df.index).round(1)
    long_df["TRAVEL_DISTANCE_KM"] = km.where(long_df["IS_AWAY"].eq(1) & prev_lat.notna() &
                                             prev_lon.notna() & lat.notna() & lon.notna())
    return long_df


def main() -> None:
    all_seasons = build_season_list(FIRST_SEASON, current_nba_season())
    current     = current_nba_season()
    to_fetch = [s for s in all_seasons
                if not (OUTPUT_DIR / f"nba_schedule_{s}.csv").exists() or s == current]
    log.info("Seasons to fetch: %s", to_fetch or "none")

    for i, season in enumerate(to_fetch):
        log.info("Season %s  (%d/%d)", season, i + 1, len(to_fetch))
        raw = fetch_schedule(season)
        time.sleep(random.uniform(2.0, 4.0))
        if raw is None or raw.empty:
            continue
        team_games = add_team_game_features(enrich_schedule(raw))
        team_games.insert(0, "SEASON", season)
        team_games.to_csv(OUTPUT_DIR / f"nba_schedule_{season}.csv", index=False)
        log.info("[%s] Saved %d team-game rows", season, len(team_games))
        time.sleep(6)

    frames = [pd.read_csv(OUTPUT_DIR / f"nba_schedule_{s}.csv", low_memory=False)
              for s in all_seasons if (OUTPUT_DIR / f"nba_schedule_{s}.csv").exists()]
    if frames:
        pd.concat(frames, ignore_index=True).to_csv(
            OUTPUT_DIR / "nba_schedule_all_seasons.csv", index=False)
        log.info("Combined -> %d rows, %d seasons", sum(map(len, frames)), len(frames))


if __name__ == "__main__":
    main()

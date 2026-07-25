# -*- coding: utf-8 -*-
"""
NBA Schedule & Travel Scraper
=============================
This builds the team-level schedule context my thesis uses for congestion and
travel: for every team-game I work out rest days, whether it was a back-to-back,
how far the team flew to get there, and whether it was a late local tip-off.

I pull the full season schedule from ScheduleLeagueV2, attach each arena's
latitude/longitude/timezone from a lookup table, then expand it to one row per
team per game and compute the rest/travel features. Player-level rolling windows
and road-trip counters are built later in R — this script just lays down the
per-game facts they need.

I keep pre-season / all-star / playoff rows in the file on purpose so the rest
and travel gaps carry across them; R filters down to regular-season games (game
ID prefix "002") when it builds the panel.

Output : nba_schedule/nba_schedule_{season}.csv       (one per season)
         nba_schedule/nba_schedule_all_seasons.csv     (combined)

It is incremental — existing season files are skipped, the current season is
always refreshed.

Requires: pip install nba_api pandas
"""

import time
import random
import logging
from datetime import datetime
from math import radians, sin, cos, sqrt, atan2
from pathlib import Path
from zoneinfo import ZoneInfo

import pandas as pd
from nba_api.stats.endpoints import scheduleleaguev2

# --- Configuration -----------------------------------------------------------

FIRST_SEASON = "2015-16"
ROOT_DIR     = Path(__file__).resolve().parent.parent   # repo root (parent of python/)
OUTPUT_DIR   = ROOT_DIR / "data" / "raw" / "nba_schedule"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

DELAY_MIN, DELAY_MAX = 2.0, 4.0
INTER_SEASON_PAUSE   = 6

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s  %(levelname)-7s  %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger("nba_schedule")

# Arena lookup: city name -> (lat, lon, IANA timezone). Includes relocated and
# historical names, plus the international / neutral venues that turned up in my
# 2021-26 window. A city missing here produces null travel, so the run warns me.
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
    "Inglewood":           (33.9451, -118.3431, "America/Los_Angeles"),  # Intuit Dome (Clippers 2024+)
    "Austin":              (30.2030, -97.7220,  "America/Chicago"),       # Moody Center
    "Paris":               (48.8939, 2.3567,    "Europe/Paris"),
    "London":              (51.5550, -0.0173,   "Europe/London"),
    "Berlin":              (52.5063, 13.4432,   "Europe/Berlin"),
    "Mexico City":         (19.4061, -99.0907,  "America/Mexico_City"),
    "Mexico City, Mexico": (19.4061, -99.0907,  "America/Mexico_City"),
}


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


def missing_seasons(all_seasons: list[str]) -> list[str]:
    """Seasons with no CSV yet; the current season is always refreshed."""
    current = current_nba_season()
    return [s for s in all_seasons
            if not (OUTPUT_DIR / f"nba_schedule_{s}.csv").exists() or s == current]


# --- Geometry ----------------------------------------------------------------

def get_arena_info(city: str):
    """(lat, lon, tz) for an arena city, with a fuzzy substring fallback."""
    if not city:
        return None
    if city in ARENA_LOOKUP:
        return ARENA_LOOKUP[city]
    for key, val in ARENA_LOOKUP.items():
        if key.lower() in city.lower() or city.lower() in key.lower():
            return val
    return None


def haversine_km(lat1, lon1, lat2, lon2) -> float:
    """Great-circle distance in km between two lat/lon points."""
    dlat, dlon = radians(lat2 - lat1), radians(lon2 - lon1)
    a = sin(dlat / 2) ** 2 + cos(radians(lat1)) * cos(radians(lat2)) * sin(dlon / 2) ** 2
    return 6371.0 * 2 * atan2(sqrt(a), sqrt(1 - a))


# --- Fetch + feature engineering ---------------------------------------------

def fetch_schedule(season: str) -> "pd.DataFrame | None":
    """Fetch the full ScheduleLeagueV2 schedule for a season (all game types)."""
    for attempt in range(1, 4):
        try:
            df = scheduleleaguev2.ScheduleLeagueV2(season=season, league_id="00").get_data_frames()[0]
            log.info("[%s] Schedule: %d games", season, len(df))
            return df
        except Exception as exc:
            log.warning("[%s] Schedule attempt %d/3 failed: %s", season, attempt, exc)
            if attempt < 3:
                time.sleep(random.uniform(5 * attempt, 10 * attempt))
    log.error("[%s] Schedule — all retries exhausted.", season)
    return None


def enrich_schedule(schedule: pd.DataFrame) -> pd.DataFrame:
    """Keep the columns I need, attach arena lat/lon, and flag late local tip-offs."""
    keep = [
        "gameId", "gameDate", "gameDateTimeUTC", "monthNum", "weekNumber", "arenaCity",
        "homeTeam_teamId", "homeTeam_teamTricode",
        "awayTeam_teamId", "awayTeam_teamTricode",
    ]
    df = schedule[[c for c in keep if c in schedule.columns]].copy()
    df["gameDateTimeUTC"] = pd.to_datetime(df["gameDateTimeUTC"], utc=True, errors="coerce")
    df["gameDate"] = pd.to_datetime(df["gameDate"], errors="coerce").dt.date

    df["ARENA_LAT"] = None
    df["ARENA_LON"] = None
    df["IS_LATE_GAME"] = None

    # Row-wise because the timezone conversion is per-arena. Runs only on new
    # seasons (the incremental gate), so this loop is cheap in practice.
    unmapped: dict[str, int] = {}
    for idx, row in df.iterrows():
        info = get_arena_info(str(row.get("arenaCity", "")))
        if not info:
            unmapped[str(row.get("arenaCity", ""))] = unmapped.get(str(row.get("arenaCity", "")), 0) + 1
            continue
        lat, lon, tz = info
        df.at[idx, "ARENA_LAT"] = lat
        df.at[idx, "ARENA_LON"] = lon
        if pd.notna(row["gameDateTimeUTC"]):
            local = row["gameDateTimeUTC"].astimezone(ZoneInfo(tz))
            df.at[idx, "IS_LATE_GAME"] = int(local.hour >= 21)   # tip-off at/after 9pm local

    # Only warn about regular-season venues (game ID prefix 002) — those reach
    # the panel, so a missing lookup entry there means missing travel data.
    reg_unmapped = {
        c: n for c, n in unmapped.items()
        if ((df["arenaCity"].astype(str) == c) &
            (df["gameId"].astype(str).str[:3] == "002")).any()
    }
    if reg_unmapped:
        log.warning("Unmapped regular-season arenaCity (add to ARENA_LOOKUP): %s",
                    ", ".join(f"{c} (x{n})" for c, n in sorted(reg_unmapped.items())))
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

    long_df = pd.concat([side(1), side(0)], ignore_index=True)
    long_df["gameDate_dt"] = pd.to_datetime(long_df["gameDate"])
    long_df = long_df.sort_values(["TEAM_ID", "gameDateTimeUTC"]).reset_index(drop=True)

    # Rest = full calendar days between games; 0 rest days => back-to-back.
    prev_date = long_df.groupby("TEAM_ID")["gameDate_dt"].shift(1)
    long_df["DAYS_REST"] = (long_df["gameDate_dt"] - prev_date).dt.days - 1
    long_df["IS_BACK_TO_BACK"] = (long_df["DAYS_REST"] == 0).astype("Int64")
    long_df.loc[prev_date.isna(), ["DAYS_REST", "IS_BACK_TO_BACK"]] = None

    # Travel = distance flown from the previous arena, only counted for away games.
    prev_lat = long_df.groupby("TEAM_ID")["ARENA_LAT"].shift(1)
    prev_lon = long_df.groupby("TEAM_ID")["ARENA_LON"].shift(1)

    def travel(r, plat, plon):
        if r["IS_AWAY"] == 0 or any(pd.isna(v) for v in (plat, plon, r["ARENA_LAT"], r["ARENA_LON"])):
            return None
        return round(haversine_km(float(plat), float(plon),
                                  float(r["ARENA_LAT"]), float(r["ARENA_LON"])), 1)

    log.info("Computing travel distances ...")
    long_df["TRAVEL_DISTANCE_KM"] = [
        travel(r, prev_lat.iloc[i], prev_lon.iloc[i]) for i, r in long_df.iterrows()
    ]
    return long_df.drop(columns=["gameDate_dt"])


def process_season(season: str) -> "pd.DataFrame | None":
    """Fetch and build the per-team-game schedule table for one season."""
    raw = fetch_schedule(season)
    time.sleep(random.uniform(DELAY_MIN, DELAY_MAX))
    if raw is None or raw.empty:
        log.warning("[%s] No schedule — skipping.", season)
        return None
    team_games = add_team_game_features(enrich_schedule(raw))
    team_games.insert(0, "SEASON", season)
    log.info("[%s] Built %d team-game rows", season, len(team_games))
    return team_games


# --- Main --------------------------------------------------------------------

def main() -> None:
    all_seasons = build_season_list(FIRST_SEASON, current_nba_season())
    log.info("Season range: %s -> %s", all_seasons[0], all_seasons[-1])

    to_fetch = missing_seasons(all_seasons)
    log.info("Seasons to fetch: %s", to_fetch or "none")

    for i, season in enumerate(to_fetch):
        log.info("=" * 55)
        log.info("Season %s  (%d/%d)", season, i + 1, len(to_fetch))
        df = process_season(season)
        if df is not None and not df.empty:
            df.to_csv(OUTPUT_DIR / f"nba_schedule_{season}.csv", index=False)
            log.info("[%s] Saved %d rows", season, len(df))
        if i < len(to_fetch) - 1:
            time.sleep(INTER_SEASON_PAUSE)

    # Rebuild the combined file from every season CSV on disk.
    frames = [pd.read_csv(OUTPUT_DIR / f"nba_schedule_{s}.csv", low_memory=False)
              for s in all_seasons if (OUTPUT_DIR / f"nba_schedule_{s}.csv").exists()]
    if frames:
        combined = pd.concat(frames, ignore_index=True)
        combined.to_csv(OUTPUT_DIR / "nba_schedule_all_seasons.csv", index=False)
        log.info("Combined saved -> %d rows, %d seasons", len(combined), len(frames))
    else:
        log.error("No season files found — combined file not written.")


if __name__ == "__main__":
    main()

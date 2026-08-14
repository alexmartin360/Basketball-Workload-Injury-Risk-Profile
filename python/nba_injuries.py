# -*- coding: utf-8 -*-
"""
NBA Injury Report Scraper
The outcome source. Downloads the official pre-game injury reports the league files at
:00 and :30 past each hour (the 1-8pm ET slots are the reliable ones) and saves them as
Parquet for the R pipeline to clean into injury events.
Incremental: timestamps already on disk are skipped, so a re-run only fills gaps.
Output: data/raw/nba_injuries/injury_reports/YYYY/MM/YYYYMMDD_HHMM.parquet
        data/raw/nba_injuries/all_reports.parquet  (merged master)
Run   : python nba_injuries.py [--start 2024-10-01] [--daily]

Requires: pip install nbainjuries pyarrow pandas tqdm
"""

import argparse
import logging
import time
from datetime import datetime, timedelta
from pathlib import Path
from zoneinfo import ZoneInfo

import pandas as pd
import pyarrow.dataset as ds
import pyarrow.parquet as pq
from tqdm import tqdm
from nbainjuries import injury

ROOT_DIR    = Path(__file__).resolve().parent.parent
BASE_DIR    = ROOT_DIR / "data" / "raw" / "nba_injuries" / "injury_reports"
MASTER_FILE = ROOT_DIR / "data" / "raw" / "nba_injuries" / "all_reports.parquet"

PROJECT_START  = datetime(2021, 10, 1)   # start of the 2021-22 modelling window
REPORT_HOURS   = range(13, 21)
REPORT_MINUTES = [0, 30]
RETRIES = 3
ET = ZoneInfo("America/New_York")

# The NBA server rejects the default urllib User-Agent.
HEADERS = {
    "User-Agent": ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
                   "(KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"),
    "Accept": "application/pdf,*/*",
    "Referer": "https://www.nba.com/",
}

logging.basicConfig(level=logging.INFO, format="%(asctime)s  %(levelname)-7s  %(message)s",
                    datefmt="%H:%M:%S")
log = logging.getLogger("nba_injuries")


def collected_timestamps() -> set[datetime]:
    """Report timestamps already on disk - this is what makes re-runs incremental."""
    stamps = set()
    for f in BASE_DIR.rglob("*.parquet"):
        try:
            stamps.add(datetime.strptime(f.stem, "%Y%m%d_%H%M"))
        except ValueError:
            pass  # not a report file
    return stamps


def candidate_timestamps(start: datetime, end: datetime) -> list[datetime]:
    """Every :00/:30 slot in the 1-8pm ET window between start and end."""
    day = start.replace(hour=0, minute=0, second=0, microsecond=0)
    stamps = []
    while day <= end:
        stamps += [day.replace(hour=h, minute=m) for h in REPORT_HOURS for m in REPORT_MINUTES]
        day += timedelta(days=1)
    return [ts for ts in stamps if start <= ts <= end]


def fetch_report(ts: datetime) -> "pd.DataFrame | None":
    """One report. None on an off-day with no games, or after the retries fail."""
    for attempt in range(1, RETRIES + 1):
        try:
            df = injury.get_reportdata(ts, return_df=True, headers=HEADERS)
            return df if (df is not None and not df.empty) else None
        except Exception as exc:
            if attempt < RETRIES:
                time.sleep(2 * attempt)
            else:
                log.debug("give up on %s: %s", ts, exc)
    return None


def save_report(df: pd.DataFrame, ts: datetime) -> None:
    out_dir = BASE_DIR / ts.strftime("%Y") / ts.strftime("%m")
    out_dir.mkdir(parents=True, exist_ok=True)
    df.assign(report_timestamp=ts, report_date=ts.date().isoformat()) \
      .to_parquet(out_dir / f"{ts:%Y%m%d_%H%M}.parquet", index=False)


def consolidate() -> None:
    """Merge every per-report Parquet into the single master file."""
    if not any(BASE_DIR.rglob("*.parquet")):
        log.warning("Nothing on disk to consolidate.")
        return
    table = ds.dataset(BASE_DIR, format="parquet").to_table()
    MASTER_FILE.parent.mkdir(parents=True, exist_ok=True)
    pq.write_table(table, MASTER_FILE, compression="snappy")
    log.info("Master file: %d rows -> %s", len(table), MASTER_FILE)


def backfill(start: datetime, end: datetime) -> None:
    already = collected_timestamps()
    todo = [ts for ts in candidate_timestamps(start, end) if ts not in already]
    log.info("On disk: %d | to fetch: %d (%s -> %s)",
             len(already), len(todo), start.date(), end.date())

    saved = 0
    for ts in tqdm(todo, desc="Fetching", unit="report", ncols=80):
        df = fetch_report(ts)
        if df is not None:
            save_report(df, ts)
            saved += 1
        time.sleep(0.5)

    log.info("Saved %d new reports (the rest were off-days).", saved)
    if saved:
        consolidate()


def daily() -> None:
    """Today's most recent report - the slot a cron job would run."""
    now = datetime.now(tz=ET).replace(tzinfo=None)
    ts = now.replace(minute=30 if now.minute >= 30 else 0, second=0, microsecond=0)
    df = fetch_report(ts)
    if df is None:
        log.info("No report at %s (likely no games today).", ts)
        return
    save_report(df, ts)
    consolidate()
    log.info("Saved today's report at %s ET.", ts)


def main() -> None:
    p = argparse.ArgumentParser(description="NBA injury report scraper (incremental).")
    p.add_argument("--start", help="Start date YYYY-MM-DD (default: 2021-10-01).")
    p.add_argument("--end", default=datetime.now().strftime("%Y-%m-%d"), help="End date YYYY-MM-DD.")
    p.add_argument("--daily", action="store_true", help="Fetch only today's latest report.")
    args = p.parse_args()

    if args.daily:
        daily()
    else:
        backfill(datetime.strptime(args.start, "%Y-%m-%d") if args.start else PROJECT_START,
                 datetime.strptime(args.end, "%Y-%m-%d"))


if __name__ == "__main__":
    main()

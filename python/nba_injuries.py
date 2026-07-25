# -*- coding: utf-8 -*-
"""
NBA Injury Report Scraper
=========================
This is my outcome source. It downloads the official NBA pre-game injury
reports (the PDFs the league files before every game) and saves them as Parquet
for the R pipeline to clean and turn into injury events.

How it works
------------
The league files a report at :00 and :30 past each hour, and the 1-8pm ET slots
are the reliable ones. For every day from the start of my modelling window to
today I try each of those slots. I save one Parquet per report, then merge them
all into a single master file.

It is incremental: before fetching I scan what is already on disk and skip those
timestamps, so re-running only fills gaps and picks up new days. The heavy
back-fill only happens once.

Output : nba_injuries/injury_reports/YYYY/MM/YYYYMMDD_HHMM.parquet  (one per report)
         nba_injuries/all_reports.parquet                          (merged master)

Run    : python nba_injuries.py               # back-fill window, then merge
         python nba_injuries.py --daily        # just today's latest report (cron)
         python nba_injuries.py --start 2024-10-01

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

# --- Configuration -----------------------------------------------------------

ROOT_DIR    = Path(__file__).resolve().parent.parent   # repo root (parent of python/)
BASE_DIR    = ROOT_DIR / "data" / "raw" / "nba_injuries" / "injury_reports"
MASTER_FILE = ROOT_DIR / "data" / "raw" / "nba_injuries" / "all_reports.parquet"

# Start of my 2021-22 modelling window. A fresh run back-fills from here.
PROJECT_START = datetime(2021, 10, 1)

# Reports land at :00 and :30, and I only bother with the reliable 1-8pm ET slots.
REPORT_MINUTES = [0, 30]
REPORT_HOURS   = range(13, 21)

RETRIES     = 3      # attempts per timestamp before I give up on it
DELAY       = 0.5    # polite pause between requests (seconds)
ET          = ZoneInfo("America/New_York")

# A browser-like header — the NBA server rejects the default urllib User-Agent.
HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
    ),
    "Accept": "application/pdf,*/*",
    "Referer": "https://www.nba.com/",
}

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s  %(levelname)-7s  %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger("nba_injuries")


# --- Helpers -----------------------------------------------------------------

def collected_timestamps() -> set[datetime]:
    """Timestamps already saved to disk — this is what makes re-runs incremental."""
    if not BASE_DIR.exists():
        return set()
    got = set()
    for f in BASE_DIR.rglob("*.parquet"):
        try:
            got.add(datetime.strptime(f.stem, "%Y%m%d_%H%M"))
        except ValueError:
            pass  # ignore stray files that aren't a report
    return got


def candidate_timestamps(start: datetime, end: datetime) -> list[datetime]:
    """Every :00/:30 slot in the 1-8pm ET window between start and end."""
    stamps, day = [], start.replace(hour=0, minute=0, second=0, microsecond=0)
    while day <= end:
        for hour in REPORT_HOURS:
            for minute in REPORT_MINUTES:
                ts = day.replace(hour=hour, minute=minute)
                if start <= ts <= end:
                    stamps.append(ts)
        day += timedelta(days=1)
    return stamps


def fetch_report(ts: datetime) -> "pd.DataFrame | None":
    """Fetch one report. Returns None on an off-day (no games) or after retries fail."""
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
    """Save one report to injury_reports/YYYY/MM/YYYYMMDD_HHMM.parquet."""
    out_dir = BASE_DIR / ts.strftime("%Y") / ts.strftime("%m")
    out_dir.mkdir(parents=True, exist_ok=True)
    df = df.copy()
    df["report_timestamp"] = ts
    df["report_date"]      = ts.date().isoformat()
    df.to_parquet(out_dir / (ts.strftime("%Y%m%d_%H%M") + ".parquet"), index=False)


def consolidate() -> None:
    """Merge every per-report Parquet into the single master file R reads."""
    if not any(BASE_DIR.rglob("*.parquet")):
        log.warning("Nothing on disk to consolidate.")
        return
    table = ds.dataset(BASE_DIR, format="parquet").to_table()
    MASTER_FILE.parent.mkdir(parents=True, exist_ok=True)
    pq.write_table(table, MASTER_FILE, compression="snappy")
    log.info("Master file: %d rows -> %s", len(table), MASTER_FILE)


# --- Collection --------------------------------------------------------------

def backfill(start: datetime, end: datetime) -> None:
    """Fetch every missing report between start and end, then rebuild the master."""
    already = collected_timestamps()
    todo    = [ts for ts in candidate_timestamps(start, end) if ts not in already]
    log.info("On disk: %d | to fetch: %d (%s -> %s)",
             len(already), len(todo), start.date(), end.date())

    if not todo:
        log.info("Nothing new — all reports already collected.")
        return

    saved = 0
    for ts in tqdm(todo, desc="Fetching", unit="report", ncols=80):
        df = fetch_report(ts)
        if df is not None:
            save_report(df, ts)
            saved += 1
        time.sleep(DELAY)

    log.info("Saved %d new reports (the rest were off-days with no games).", saved)
    if saved:
        consolidate()


def daily() -> None:
    """Grab today's most recent report — the slot I'd run from a cron job."""
    now = datetime.now(tz=ET).replace(tzinfo=None)
    ts  = now.replace(minute=30 if now.minute >= 30 else 0, second=0, microsecond=0)
    df  = fetch_report(ts)
    if df is not None:
        save_report(df, ts)
        consolidate()
        log.info("Saved today's report at %s ET.", ts)
    else:
        log.info("No report at %s (likely no games today).", ts)


# --- CLI ---------------------------------------------------------------------

def main() -> None:
    p = argparse.ArgumentParser(description="NBA injury report scraper (incremental).")
    p.add_argument("--start", help="Start date YYYY-MM-DD (default: 2021-10-01).")
    p.add_argument("--end", default=datetime.now().strftime("%Y-%m-%d"),
                   help="End date YYYY-MM-DD (default: today).")
    p.add_argument("--daily", action="store_true", help="Fetch only today's latest report.")
    args = p.parse_args()

    if args.daily:
        daily()
        return

    start = datetime.strptime(args.start, "%Y-%m-%d") if args.start else PROJECT_START
    end   = datetime.strptime(args.end, "%Y-%m-%d")
    backfill(start, end)


if __name__ == "__main__":
    main()

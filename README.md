# Basketball-Workload-Injury-Risk-Profile

Investigates how patterns of matches, training load, recovery relate to injury risk in basketball. Quantify how changes in workload accumulation, cumulative effects, schedule influence likelihood of injury that could lead to reduction, using time to event modelling. Designed to produce evidence that can inform applied load management decisions.

Master of Sports Analytics thesis (La Trobe University) — *Workload, Schedule Congestion,
and Injury Risk in the NBA: A Time-to-Event Perspective*. Five NBA seasons (2021-22 → 2025-26).

## Repository layout

```
├── python/         # NBA data collectors (injury reports, schedule, travel, tracking load)
├── analysis/       # R pipeline — scripts 1-5 build the data, 6 (.Rmd) models & reports
│   └── Archive/    # superseded pre-.Rmd scripts, kept for reference (gitignored)
├── docs/           # rendered outputs (knitted HTML report + supporting PDF)
├── data/           # generated data (gitignored) — raw/ = scraped, processed/ = built .rds
├── requirements.txt
└── *.Rproj         # RStudio project — open at the repo root
```

Paths are anchored at the repo root (`here::here()` in R, `__file__`-relative in Python),
so the project runs from any location with no edits.

## Pipeline

**1. Collect** (Python, from the repo root):

```bash
python3 -m venv .venv && source .venv/bin/activate
pip install -r requirements.txt
python python/nba_injuries.py         # official injury report PDFs → parquet
python python/nba_schedule.py         # league schedule + travel distances
python python/nba_player_travel.py    # game logs, rosters, career stats
python python/nba_player_tracking.py  # player tracking load (distance)
```

Outputs land in `data/raw/`.

**2. Build the modelling data** (R): open the `.Rproj` at the repo root, then run
`analysis/1` → `analysis/5` in order. These write `data/processed/cox_ready_2021-26.rds`.

**3. Model & report** (R): knit `analysis/6. Cox Modelling.Rmd` — all Cox modelling,
diagnostics and validation. Knitting from RStudio renders the report into `docs/`.

## The report

The knitted analysis is [`docs/6.-Cox-Modelling.html`](docs/6.-Cox-Modelling.html): Cox
proportional-hazards modelling of workload and schedule congestion against injury risk,
with proportional-hazards diagnostics, a recurrent-events robustness suite (Andersen-Gill,
PWP gap-time, shared frailty), an injury-mechanism split, and out-of-sample validation
(temporal + player-grouped cross-validation).

## Requirements

- **R**: `tidyverse`, `survival`, `broom`, `coxme`, `lubridate`, `slider`, `arrow`, `fs`,
  `here`, plus `rmarkdown` / `knitr` / `kableExtra` for the report.
- **Python**: see [`requirements.txt`](requirements.txt).

The `data/` directory is gitignored — rebuild it from the pipeline above.

# Baseline model design choices in collaborative forecast hubs

Code for *"Basic baseline model design choices can substantially influence performance in collaborative forecast
hubs"*. It makes retrospective forecasts with trend (flatline and drift) and seasonal baseline models for COVID-19,
influenza and RSV hospital admissions and weighted influenza-like illness (wILI), scores them alongside real-time
forecast hub submissions, and produces the figures, tables and statistics in the manuscript.

## Requirements

R 4.4.1. Package versions are recorded in `pipeline/renv.lock`:

```r
install.packages("renv")
renv::restore(lockfile = "pipeline/renv.lock")
```

The data downloads also need `git`, `curl` and `python3`. The Delphi Epidata API limits anonymous access to about 60
requests per hour; setting `DELPHI_EPIDATA_KEY` removes the limit.

All scripts are run from the repository root.

## 1. Download the data

All input data are public. Repository downloads are pinned to specific commits, and the exact snapshots used are
listed in `pipeline/data_manifest/`. Files are written to `data/`.

```bash
Rscript pipeline/scripts/fetch_data.R                    # locations, COVID-19, influenza, RSV-NET and wILI
Rscript pipeline/scripts/fetch_ili_versions.R            # versioned wILI
Rscript pipeline/scripts/fetch_hub_forecasts.R           # COVID-19 Forecast Hub and FluSight submissions
Rscript pipeline/scripts/fetch_rsv_nhsn.R                # CDC RSV Forecast Hub data and submissions
Rscript pipeline/scripts/fetch_flu_hub_versions.R        # influenza data as FluSight had it each week
Rscript pipeline/scripts/fetch_rsv_nhsn_hub_versions.R   # RSV data as the RSV Forecast Hub had it each week
Rscript pipeline/scripts/covid_hub_vintage.R             # COVID-19 data as the COVID-19 Forecast Hub had it each week
```

## 2. Run the analysis

```bash
Rscript pipeline/run_all.R
```

This takes about 3 hours on 8 cores and writes to `pipeline/output/`. Forecast runs are checkpointed and resume
where they stopped. Every forecast uses a fixed seed, so results do not depend on the number of cores.

| Script | Output |
|---|---|
| `01_build_data.R` | Forecast tasks and final data for each target: `output/vintaged/` |
| `02_run_trend.R` | Trend baseline forecasts for every model configuration, scored: `output/scores/`, `output/quantiles/` |
| `03_score_hubs.R` | Hub submissions scored on the same forecasts: `output/scores/hub_<target>.parquet` |
| `04_run_seasonal.R` | Seasonal baseline forecasts for wILI: `output/scores/ili_asof_seasonal.parquet` |
| `05_analysis.R` | Summary tables: `output/tables/` |
| `06_rsv_data_versions.R` | RSV forecasts from real-time and final data: `output/tables/reporting_scores_by_version.csv` |
| `07_wili_long_horizons.R` | wILI flatline vs seasonal baseline, 1-10 weeks ahead: `output/tables/wili_long_horizon_summary.csv` |
| `08_log_scale_scoring.R` | Scores on the log scale: `output/tables/*_log_scale.csv` |

## 3. Make the figures, tables and statistics

```bash
Rscript manuscript/make_all.R             # figures and tables -> manuscript/output/
Rscript manuscript/summary_statistics.R   # numbers in the text -> manuscript/output/summary_statistics.csv
```

`manuscript/items.csv` lists the script that makes each figure and table.

## Code

```
pipeline/R/config.R          model grid, quantiles, targets, evaluation periods, seeds, hub settings
pipeline/R/data.R            forecast tasks from the raw data
pipeline/R/baseline_model.R  trend baseline (flatline and drift)
pipeline/R/seasonal_model.R  seasonal baseline
pipeline/R/scoring.R         weighted interval score and interval coverage
pipeline/R/hubs.R            matching and scoring hub submissions
pipeline/R/runner.R          running and scoring the model grid
pipeline/R/analysis.R        loading scores and comparing models
pipeline/scripts/            analysis steps and data downloads
manuscript/                  figures, tables and statistics
```

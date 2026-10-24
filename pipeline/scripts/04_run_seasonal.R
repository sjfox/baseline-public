# Rolling-origin seasonal-baseline forecasts for wILI, scored like the trend models.
# Origins: every wILI trend-model origin from SEASONAL_START onward.
# Fit to the versioned wILI data available at each origin (ili_asof).
# Usage: Rscript pipeline/scripts/04_run_seasonal.R [target ...]
source("pipeline/R/setup.R")
source("pipeline/R/seasonal_model.R")

args <- commandArgs(TRUE)
targets <- if (length(args)) args else "ili_asof"
# Main specification (log, all data) and the same model without the log transform, each trained on
# all data and on only the most recent 1-10 years.
variants <- c(list(seasonal       = list(),
                   seasonal_nolog = list(transform = "none")),
              setNames(lapply(1:10, function(n) list(n_years = n)), paste0("seasonal_last", 1:10, "y")),
              setNames(lapply(1:10, function(n) list(transform = "none", n_years = n)),
                       paste0("seasonal_nolog_last", 1:10, "y")))

# Training data for one task: the series as available at its origin, on its weekly grid.
task_train <- function(task) {
  y <- task$series[[1]]
  data.table(date = task$reference_date - 7L * rev(seq_along(y) - 1L), value = y)
}

run_origin <- function(o, tasks, truth, target) {
  td <- o + 7L * HORIZONS_WEEKLY
  sc <- list(); qs <- list()
  for (loc in sort(unique(tasks[reference_date == o, location]))) {
    tr  <- task_train(tasks[reference_date == o & location == loc])
    obs <- truth[.(loc, td), value]
    for (v in names(variants)) {
      # own seed per location and variant, so adding or dropping a variant changes no other forecast
      set.seed(task_seed(paste(target, v), loc, o))
      q <- do.call(seasonal_forecast, c(list(train = tr, target_dates = td), variants[[v]]))
      sc[[length(sc) + 1]] <- data.table(target = target, location = loc, reference_date = o,
        model = v, horizon = HORIZONS_WEEKLY, target_end_date = td, observed = obs,
        score_quantiles(q, obs), wis_log = wis_log_scale(q, obs))
      qs[[length(qs) + 1]] <- data.table(target = target, location = loc, reference_date = o,
        model = v, horizon = rep(HORIZONS_WEEKLY, length(QUANTILES)),
        target_end_date = rep(td, length(QUANTILES)),
        quantile = rep(QUANTILES, each = length(td)), value = as.vector(q))
    }
  }
  list(scores = rbindlist(sc), quantiles = rbindlist(qs))
}

for (target in targets) {
  d <- load_target(target)
  truth <- d$truth[!is.na(value)]
  setkey(truth, location, date)
  tasks <- d$tasks[reference_date >= SEASONAL_START]
  origins <- sort(unique(tasks$reference_date))
  res <- parallel::mclapply(origins, run_origin, tasks = tasks, truth = truth, target = target,
                            mc.cores = as.integer(Sys.getenv("NCORES", N_CORES)))
  stopifnot(all(vapply(res, is.list, logical(1))))
  scores <- rbindlist(lapply(res, `[[`, "scores"))
  quants <- rbindlist(lapply(res, `[[`, "quantiles"))
  arrow::write_parquet(scores, file.path(OUT_DIR, "scores", paste0(target, "_seasonal.parquet")))
  arrow::write_parquet(quants, file.path(OUT_DIR, "quantiles", paste0(target, "_seasonal.parquet")))
  message("Seasonal (", target, "): ", nrow(scores), " score rows for ", length(origins), " origins")
}

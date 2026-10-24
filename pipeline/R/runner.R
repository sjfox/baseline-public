# Run the full trend-baseline grid for one target, score it, and checkpoint to disk.

library(data.table)

model_grid <- function(windows) {
  g <- CJ(transformation = TRANSFORMATIONS, symmetrize = SYMMETRIZE, window = windows,
          sorted = FALSE)
  g[, model := model_id(transformation, symmetrize, window)]
  g[]
}

# Deterministic per-task seed (independent of core count and task order).
task_seed <- function(target, location, reference_date) {
  key <- paste(target, location, reference_date)
  h <- sum(utf8ToInt(key) * seq_along(utf8ToInt(key))) %% 1e6
  as.integer(BASE_SEED + h + as.integer(reference_date))
}

# Forecast + score one task across the model grid (all windows batched per variant).
run_task <- function(task, grid, steps, step_days, truth_lookup, count_data) {
  y <- task$series[[1]]
  ref <- task$reference_date
  target_dates <- ref + steps * step_days
  obs <- truth_lookup(task$location, target_dates)
  set.seed(task_seed(task$target, task$location, ref))
  H <- length(steps); sc <- list(); qs <- list()
  for (v in split(grid, by = c("transformation", "symmetrize"))) {
    arr <- forecast_quantiles_batch(y, v$transformation[1], v$symmetrize[1], v$window, keep = steps,
                                    count_data = count_data)
    W <- nrow(v)
    qm <- matrix(aperm(arr, c(1, 3, 2)), nrow = H * W)      # rows: horizon fastest, then window
    keep_rows <- !is.na(qm[, 1])
    s <- score_quantiles(qm[keep_rows, , drop = FALSE], rep(obs, W)[keep_rows])
    s$wis_log <- wis_log_scale(qm[keep_rows, , drop = FALSE], rep(obs, W)[keep_rows])
    sc[[length(sc) + 1]] <- data.table(model = rep(v$model, each = H)[keep_rows],
                                       horizon = rep(seq_len(H), W)[keep_rows],
                                       target_end_date = rep(target_dates, W)[keep_rows],
                                       observed = rep(obs, W)[keep_rows], s)
    sv <- which(v$window %in% SAVE_QUANTILE_WINDOWS)
    for (j in sv) qs[[length(qs) + 1]] <- data.table(
      model = v$model[j], horizon = rep(seq_len(H), length(QUANTILES)),
      target_end_date = rep(target_dates, length(QUANTILES)),
      quantile = rep(QUANTILES, each = H), value = as.vector(arr[, , j]))
  }
  meta <- list(target = task$target, snapshot = task$snapshot, location = task$location,
               reference_date = ref, n_obs = task$n_obs)
  list(scores = cbind(as.data.table(meta), rbindlist(sc)),
       quantiles = cbind(as.data.table(meta), rbindlist(qs)))
}

run_target <- function(target, cores = N_CORES, overwrite = FALSE) {
  dat <- load_target(target)
  daily <- dat$step == 1
  steps <- if (daily) HORIZONS_DAILY else HORIZONS_WEEKLY
  grid  <- model_grid(if (daily) WINDOWS_DAILY else WINDOWS_WEEKLY)
  tasks <- dat$tasks
  message(sprintf("[%s] %d tasks x %d models (%s)", target, nrow(tasks), nrow(grid), Sys.time()))

  truth <- dat$truth[!is.na(value)]
  setkey(truth, location, date)
  truth_lookup <- function(loc, dates) truth[.(loc, dates), value]

  out_dir <- file.path(OUT_DIR, "runs", target); dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  # One chunk per forecast origin date (all locations) -> restartable checkpoints.
  chunks <- split(tasks, by = "reference_date")
  todo <- names(chunks)
  if (!overwrite) todo <- todo[!file.exists(file.path(out_dir, paste0(todo, ".rds")))]
  message(sprintf("[%s] %d/%d origin dates to run on %d cores", target, length(todo), length(chunks), cores))

  res <- parallel::mclapply(todo, function(d) {
    data.table::setDTthreads(1L)   # avoid oversubscription: parallelism comes from the forked workers
    ch <- chunks[[d]]
    r <- lapply(seq_len(nrow(ch)), function(j)
      run_task(ch[j], grid, steps, dat$step, truth_lookup, dat$count_data))
    out <- list(scores = rbindlist(lapply(r, `[[`, "scores")),
                quantiles = rbindlist(lapply(r, `[[`, "quantiles")))
    saveRDS(out, file.path(out_dir, paste0(d, ".rds")))
    cat(sprintf("[%s] %s done %s\n", target, d, format(Sys.time(), "%H:%M:%S")),
        file = file.path(OUT_DIR, "logs", paste0(target, ".log")), append = TRUE)
    TRUE
  }, mc.cores = cores, mc.preschedule = FALSE)
  failed <- todo[!vapply(res, isTRUE, logical(1))]
  if (length(failed)) stop("Failed chunks for ", target, ": ", paste(failed, collapse = ", "))
  collect_target(target)
}

# Combine checkpoints into one scores file and one quantiles file per target.
collect_target <- function(target) {
  files <- list.files(file.path(OUT_DIR, "runs", target), pattern = "\\.rds$", full.names = TRUE)
  parts <- lapply(files, readRDS)
  scores <- rbindlist(lapply(parts, `[[`, "scores"))
  quants <- rbindlist(lapply(parts, `[[`, "quantiles"))
  arrow::write_parquet(scores, file.path(OUT_DIR, "scores", paste0(target, ".parquet")))
  arrow::write_parquet(quants, file.path(OUT_DIR, "quantiles", paste0(target, ".parquet")))
  message(sprintf("[%s] collected %d score rows", target, nrow(scores)))
  invisible(scores)
}

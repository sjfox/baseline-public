# Build the vintaged data for every target from the raw downloads in data/ and write it to
# output/vintaged/ (file description in R/data.R). Every later step reads these files through
# load_target(), so this is the only step that reads the raw forecast data.
# Writes: output/vintaged/<target>_vintages.parquet, <target>_truth.parquet, targets.csv (summary),
#         raw_data_md5.csv (checksums of every file in data/)
source("pipeline/R/setup.R")
summ <- rbindlist(lapply(ALL_TARGETS, function(tg) {
  d <- write_target(tg); t <- d$tasks
  message(sprintf("[%s] %d forecast tasks written", tg, nrow(t)))
  data.table(target = tg, step = d$step, count_data = d$count_data,
             n_tasks = nrow(t), n_locations = uniqueN(t$location), n_origins = uniqueN(t$reference_date),
             first_origin = min(t$reference_date), last_origin = max(t$reference_date),
             n_snapshots = uniqueN(t$snapshot), min_obs = min(t$n_obs), max_obs = max(t$n_obs),
             origin_lag_days_max = as.numeric(max(t$snapshot - t$reference_date)),
             locations = paste(sort(unique(t$location)), collapse = " "))
}))
fwrite(summ, file.path(VINTAGED_DIR, "targets.csv"))
print(summ[, -"locations"])

# Checksums of the raw inputs, tying the vintaged files to the exact downloads they came from
files <- list.files(DATA_DIR, recursive = TRUE, full.names = TRUE)
fwrite(data.table(file = sub(paste0("^", DATA_DIR, "/"), "", files), bytes = file.size(files),
                  md5 = unname(tools::md5sum(files))),
       file.path(VINTAGED_DIR, "raw_data_md5.csv"))
message("Vintaged data for ", nrow(summ), " targets and checksums of ", length(files), " raw files in ", VINTAGED_DIR)

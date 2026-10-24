# Fetch versioned (as-of) wILI from the Delphi Epidata API (fluview), one issue per call.
# Each issue: all 11 regions, the 105 most recent epiweeks as reported in that issue.
# Output: data/ili/archive/ili_versions.csv (issue, region, epiweek_start, lag, wili).
# Anonymous API access is sufficient (one issue per request); the run is resumable.
suppressMessages({ library(epidatr); library(data.table) })
options(epidatr.cache = FALSE)
out <- "data/ili/archive/ili_versions.csv"
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
regions <- c("nat", paste0("hhs", 1:10))
truth <- fread("data/ili/truth_ili_seasonal.csv")
origins <- sort(unique(as.Date(truth$target_end_date)))
origins <- origins[origins >= as.Date("2014-01-01") & origins <= as.Date("2023-05-31") &
                   as.integer(format(origins, "%m")) %in% c(10:12, 1:4)]   # in-season origins only
to_ew <- function(d) { m <- MMWRweek::MMWRweek(d); m$MMWRyear * 100 + m$MMWRweek }
issues <- unique(to_ew(origins))
done <- if (file.exists(out)) unique(fread(out)$issue_ew) else integer(0)
todo <- setdiff(issues, done)
message(length(todo), " of ", length(issues), " issues to fetch")
for (iss in todo) {
  d_iss <- MMWRweek::MMWRweek2Date(iss %/% 100, iss %% 100)
  start <- to_ew(d_iss - 7 * 104)
  x <- NULL
  for (attempt in 1:5) {
    x <- tryCatch(pub_fluview(regions = regions, epiweeks = epirange(start, iss), issues = iss),
                  error = function(e) { message("retry ", iss, ": ", conditionMessage(e)); NULL })
    if (!is.null(x)) break
    Sys.sleep(10 * attempt)
  }
  if (is.null(x)) stop("failed to fetch issue ", iss)
  x <- as.data.table(x)[, .(issue_ew = iss, region, epiweek_start = as.Date(epiweek), lag, wili)]
  fwrite(x, out, append = file.exists(out))
  Sys.sleep(0.5)
}
message("done: ", nrow(fread(out)), " rows")

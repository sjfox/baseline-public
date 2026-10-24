# Table 1: the square-root flatline baseline with w = 10 and with all differences (w = infinity), for each
# main target, compared with alternatives. Each % column is the % improvement in mean WIS of the row's model
# over the alternative: (WIS_alternative - WIS_model) / WIS_alternative x 100, over identical forecasts
# (positive = the row's model is better).
#   vs drift:            drift, same window and transformation (square root)
#   vs raw data:         flatline, same window, untransformed data
#   vs log data (WIS):   flatline, same window, log-transformed data
#   vs log data (log WIS): the same comparison scored with log-scale WIS (WIS of log(x + 1))
#   vs standard baseline: the hub's real-time baseline submissions on the forecasts it made
#                         (COVID-19, influenza, RSV), or the seasonal baseline (wILI)
#   rank / percentile among submitted models (hub targets only; wILI had no real-time hub): each team
#     model covering >= 50% of our forecasts is compared head-to-head with the row's model on the
#     forecasts both made (ratio of mean WIS, as in pipeline/scripts/05_analysis.R). Rank = 1 + number of
#     team models with lower WIS, out of the team models plus ours; "outperformed" = % of team models with
#     higher WIS. Hub-produced models (hub baseline, hub ensemble) are not counted.
# Reads:  pipeline/output/scores/<target>.parquet, hub_<target>.parquet;
#         pipeline/output/tables/variants_vs_hub_baseline.csv, seasonal_comparison_scores.csv
# Writes: manuscript/output/Table_1.csv (number from items.csv)
source("manuscript/_setup.R")

TARGET_ROWS <- c(covid = "COVID-19", flu = "Influenza", rsv_nhsn = "RSV", ili_asof = "wILI")
WINDOWS     <- c(w10 = "10", wall = "∞")
MIN_COVERAGE <- 0.5     # team models included in the rank: those covering >= 50% of our forecasts
DIGITS      <- 2

pct <- function(model_wis, alt_wis) (alt_wis - model_wis) / alt_wis * 100
fmt <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = DIGITS))
rel_hb <- tab("variants_vs_hub_baseline.csv")
seasonal_wis <- tab("seasonal_comparison_scores.csv")[model == "seasonal", mean(wis)]   # wILI, same forecasts

rows <- list()
for (t in names(TARGET_ROWS)) {
  models <- as.vector(outer(c("sqrt_sym_", "sqrt_nonsym_", "none_sym_", "log_sym_"), names(WINDOWS), paste0))
  s <- load_scores(t, models = models)
  m <- s[, .(wis = mean(wis), wis_log = mean(wis_log)), by = model]
  hub <- t %in% names(HUB_BASELINE)
  if (hub) {   # team models on exactly the forecasts we score, and their coverage
    cells <- unique(s[, .(location, reference_date, horizon)])
    h <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "scores", paste0("hub_", t, ".parquet"))))[
      cells, on = .(location, reference_date, horizon), nomatch = 0][!grepl(HUB_OWN_MODELS, model)]
    team <- h[, .(coverage = .N / nrow(cells)), by = model][coverage >= MIN_COVERAGE, model]
    h <- h[model %in% team, .(model, location, reference_date, horizon, wis)]
  }
  for (w in names(WINDOWS)) {
    v <- paste0("sqrt_sym_", w)
    W <- function(id, col = "wis") m[model == paste0(id, w), get(col)]
    std <- if (hub) (1 - rel_hb[target == t & model == v, rel_wis_vs_hub_baseline]) * 100 else pct(W("sqrt_sym_"), seasonal_wis)
    rank <- pctl <- NA_real_; n_team <- NA_integer_
    if (hub) {
      ours <- s[model == v, .(location, reference_date, horizon, wis_ours = wis)]
      ratio <- merge(h, ours, by = c("location", "reference_date", "horizon"))[
        , .(rel = mean(wis) / mean(wis_ours)), by = model]$rel   # < 1: team model better
      n_team <- length(ratio); rank <- 1 + sum(ratio < 1); pctl <- 100 * mean(ratio > 1)
    }
    rows[[length(rows) + 1]] <- data.table(
      Target = TARGET_ROWS[[t]], `Window size` = WINDOWS[[w]],
      `Relative to drift (%)`                  = fmt(pct(W("sqrt_sym_"), W("sqrt_nonsym_"))),
      `Relative to raw data (%)`               = fmt(pct(W("sqrt_sym_"), W("none_sym_"))),
      `Relative to log data (WIS %)`           = fmt(pct(W("sqrt_sym_"), W("log_sym_"))),
      `Relative to log data (log WIS %)`       = fmt(pct(W("sqrt_sym_", "wis_log"), W("log_sym_", "wis_log"))),
      `Relative to standard baseline (%)`      = fmt(std),
      `Rank among submitted models`            = if (hub) sprintf("%d of %d", rank, n_team + 1) else "",
      `Submitted models outperformed (%)`      = if (hub) formatC(pctl, format = "f", digits = 0) else "")
  }
}
t1 <- rbindlist(rows)
print(t1)
save_table(t1, "focal_vs_alternatives")

# Numbers quoted in the manuscript text, one section per claim, in manuscript order.
# Reads:  pipeline/output/tables/wis_by_window.csv, wis_by_window_log_scale.csv, seasonal_variants.csv,
#         variants_vs_hub_baseline.csv, wili_long_horizon_summary.csv;
#         pipeline/output/scores/<target>.parquet, hub_<target>.parquet, ili_asof_seasonal.parquet
# Writes: manuscript/output/summary_statistics.csv
# Run from the project root: Rscript manuscript/summary_statistics.R
source("manuscript/_setup.R")

wis <- tab("wis_by_window.csv")[target %in% MAIN]
stats <- list()
add <- function(id, description, value) stats[[length(stats) + 1]] <<- data.table(id, description, value)

# Results: flatline vs drift, raw data, w = 10.
# % mean WIS improvement of the flatline over the drift = 100 * (1 - WIS flatline / WIS drift);
# negative = flatline worse.
fd <- dcast(wis[model %in% c("none_sym_w10", "none_nonsym_w10")], target ~ model, value.var = "wis")
fd[, pct := 100 * (1 - none_sym_w10 / none_nonsym_w10)]
for (t in intersect(FIG_ORDER, fd$target))
  add(paste0("flat_vs_drift_raw_w10_", t), paste0("Flatline vs drift, raw, w = 10, % WIS improvement: ", PANEL_LABEL[t]),
      fd[target == t, pct])

# Results: 95% prediction interval coverage, flatline minus drift (percentage points), raw data,
# w = 10: per target, then the mean, min and max over the four targets.
cv <- dcast(wis[model %in% c("none_sym_w10", "none_nonsym_w10")], target ~ model, value.var = "cov95")
cv[, pp := 100 * (none_sym_w10 - none_nonsym_w10)]
for (t in intersect(FIG_ORDER, cv$target))
  add(paste0("flat_vs_drift_cov95_raw_w10_", t),
      paste0("Flatline minus drift, 95% coverage (pp), raw, w = 10: ", PANEL_LABEL[t]), cv[target == t, pp])
add("flat_vs_drift_cov95_raw_w10_mean", "Flatline minus drift, 95% coverage (pp), raw, w = 10: mean", mean(cv$pp))
add("flat_vs_drift_cov95_raw_w10_min", "Flatline minus drift, 95% coverage (pp), raw, w = 10: min", min(cv$pp))
add("flat_vs_drift_cov95_raw_w10_max", "Flatline minus drift, 95% coverage (pp), raw, w = 10: max", max(cv$pp))

# Results (Figure 3): best drift model per target over raw and square-root data and every window,
# as in Figure 3 (log models are in Figure S6). For each target, the best window (Inf = all data)
# and the % WIS improvement of the best drift model over the square-root drift on all data (0 where
# that is the best); then log vs square-root drift on all data, since the log drift is better for some targets.
dr <- wis[grepl("_nonsym_", model)]
best <- dr[!grepl("^log", model), .SD[which.min(wis)], by = target]
all_wis <- function(m) dr[model == m][best, on = "target", x.wis]
best[, `:=`(sqrt_all = all_wis("sqrt_nonsym_wall"), log_all = all_wis("log_nonsym_wall"))]
for (t in intersect(FIG_ORDER, best$target)) {
  b <- best[target == t]
  add(paste0("best_drift_window_", t), paste0("Best drift model (raw or sqrt) window: ", PANEL_LABEL[t], " (", b$model, ")"),
      if (b$window == "all") Inf else as.numeric(b$window))
  add(paste0("best_drift_vs_sqrt_all_", t),
      paste0("Best drift model vs square-root drift on all data, % WIS improvement: ", PANEL_LABEL[t]),
      100 * (1 - b$wis / b$sqrt_all))
  add(paste0("log_vs_sqrt_drift_all_", t),
      paste0("Log vs square-root drift on all data, % WIS improvement: ", PANEL_LABEL[t]),
      100 * (1 - b$log_all / b$sqrt_all))
}

# Results (Figure 3): square-root vs raw flatline on all data, % WIS improvement per target, then
# the mean, min and max over the four targets.
sr <- dcast(wis[model %in% c("sqrt_sym_wall", "none_sym_wall")], target ~ model, value.var = "wis")
sr[, pct := 100 * (1 - sqrt_sym_wall / none_sym_wall)]
for (t in intersect(FIG_ORDER, sr$target))
  add(paste0("sqrt_vs_raw_flat_all_", t),
      paste0("Square-root vs raw flatline, all data, % WIS improvement: ", PANEL_LABEL[t]), sr[target == t, pct])
add("sqrt_vs_raw_flat_all_mean", "Square-root vs raw flatline, all data, % WIS improvement: mean", mean(sr$pct))
add("sqrt_vs_raw_flat_all_min", "Square-root vs raw flatline, all data, % WIS improvement: min", min(sr$pct))
add("sqrt_vs_raw_flat_all_max", "Square-root vs raw flatline, all data, % WIS improvement: max", max(sr$pct))

# Results: our standard baseline (flatline, all raw data) relative to the hub's own baseline: mean
# WIS over the forecasts the hub baseline also made, divided by the hub baseline's mean WIS there
# (< 1 = ours better); and the % of our forecasts the hub baseline also made.
vh <- tab("variants_vs_hub_baseline.csv")[model == STANDARD_MODEL]
for (t in intersect(FIG_ORDER, vh$target)) {
  add(paste0("standard_vs_hub_baseline_", t),
      paste0("Our standard baseline / hub baseline, relative WIS: ", PANEL_LABEL[t]), vh[target == t, rel_wis_vs_hub_baseline])
  add(paste0("hub_baseline_pct_cells_", t),
      paste0("% of our forecasts also made by the hub baseline: ", PANEL_LABEL[t]), vh[target == t, 100 * n_cells / n_total])
}

# Results: flatline variants vs the hub baseline (relative WIS, < 1 = ours better). Per target, the
# best raw or square-root flatline (as in Figure 3), the best flatline including log, and the number
# of raw or square-root flatline variants that beat the hub baseline.
vf <- tab("variants_vs_hub_baseline.csv")[grepl("_sym_", model)]
for (t in intersect(FIG_ORDER, vf$target)) {
  d <- vf[target == t][order(rel_wis_vs_hub_baseline)]
  b <- d[!grepl("^log", model)][1]; bl <- d[1]
  add(paste0("best_flat_vs_hub_baseline_", t),
      paste0("Best raw/sqrt flatline / hub baseline, relative WIS: ", PANEL_LABEL[t], " (", b$model, ")"),
      b$rel_wis_vs_hub_baseline)
  add(paste0("best_flat_incl_log_vs_hub_baseline_", t),
      paste0("Best flatline incl. log / hub baseline, relative WIS: ", PANEL_LABEL[t], " (", bl$model, ")"),
      bl$rel_wis_vs_hub_baseline)
  add(paste0("n_flat_beat_hub_baseline_", t),
      paste0("Raw/sqrt flatline variants beating the hub baseline (of ", d[!grepl("^log", model), .N], "): ", PANEL_LABEL[t]),
      d[!grepl("^log", model), sum(rel_wis_vs_hub_baseline < 1)])
}

# Results (Table 1): % of submitted team models the square-root flatline (w = 10 and all data)
# outperforms, as in manuscript/tables/focal_vs_alternatives.R: each team model covering >= 50% of
# our forecasts is compared head-to-head on the forecasts both made (ratio of mean WIS); hub-produced
# models are not counted. Also the number outperformed and the number of team models.
for (t in intersect(FIG_ORDER, names(HUB_BASELINE))) {
  s <- load_scores(t, models = c("sqrt_sym_w10", "sqrt_sym_wall"))
  cells <- unique(s[, .(location, reference_date, horizon)])
  h <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "scores", paste0("hub_", t, ".parquet"))))[
    cells, on = .(location, reference_date, horizon), nomatch = 0][!grepl(HUB_OWN_MODELS, model)]
  h <- h[model %in% h[, .(coverage = .N / nrow(cells)), by = model][coverage >= 0.5, model]]
  for (v in c("sqrt_sym_w10", "sqrt_sym_wall")) {
    ours <- s[model == v, .(location, reference_date, horizon, wis_ours = wis)]
    ratio <- merge(h, ours, by = c("location", "reference_date", "horizon"))[
      , .(rel = mean(wis) / mean(wis_ours)), by = model]$rel   # > 1: ours better
    w <- sub("sqrt_sym_", "", v)
    add(paste0("pct_team_beaten_", v, "_", t),
        paste0("% of submitted models outperformed, sqrt flatline ", w, ": ", PANEL_LABEL[t]), 100 * mean(ratio > 1))
    add(paste0("n_team_beaten_", v, "_", t),
        paste0("Submitted models outperformed (of ", length(ratio), "), sqrt flatline ", w, ": ", PANEL_LABEL[t]),
        sum(ratio > 1))
  }
}

# Results (Table 1): how much the square-root flatline underperforms the log flatline on all data,
# scored with log-scale WIS (WIS of log(x + 1)): 100 * (log WIS sqrt model - log WIS log model) /
# log WIS log model, per target, then the mean, min and max. This is Table 1's "Relative to log data
# (log WIS %)" (w = infinity) with the sign flipped; negative = the square-root model is better.
lw <- dcast(tab("wis_by_window_log_scale.csv")[target %in% MAIN & model %in% c("log_sym_wall", "sqrt_sym_wall")],
            target ~ model, value.var = "wis")
lw[, pct := 100 * (sqrt_sym_wall - log_sym_wall) / log_sym_wall]
for (t in intersect(FIG_ORDER, lw$target))
  add(paste0("sqrt_underperforms_log_flat_all_logwis_", t),
      paste0("Square-root underperforms log flatline, all data, log WIS, %: ", PANEL_LABEL[t]), lw[target == t, pct])
add("sqrt_underperforms_log_flat_all_logwis_mean", "Square-root underperforms log flatline, all data, log WIS, %: mean", mean(lw$pct))
add("sqrt_underperforms_log_flat_all_logwis_min", "Square-root underperforms log flatline, all data, log WIS, %: min", min(lw$pct))
add("sqrt_underperforms_log_flat_all_logwis_max", "Square-root underperforms log flatline, all data, log WIS, %: max", max(lw$pct))

# Results (Figure S12): seasonal baseline (wILI) fit to the most recent 1-10 years of log data vs all
# history (the main specification), % WIS improvement per number of years.
sv <- tab("seasonal_variants.csv")
for (y in 1:10)
  add(paste0("seasonal_last", y, "y_vs_all"),
      paste0("Seasonal (log), last ", y, " years vs all history, % WIS improvement"),
      100 * (1 - sv[seasonal_variant == paste0("seasonal_last", y, "y"), seasonal_wis] /
                 sv[seasonal_variant == "seasonal", seasonal_wis]))

# Results (Figure 4): best flatline vs best seasonal baseline for wILI (real-time data), the models and
# forecasts of manuscript/figures/seasonal_vs_flatline.R: log flatline on all data vs the log seasonal
# model fit to the last 3 years. % WIS improvement of the flatline = 100 * (1 - mean WIS flatline /
# mean WIS seasonal): overall, by horizon (1-4 weeks, from Figure S13) and per season (then mean, min and max over
# seasons); % of forecast dates on which the flatline's mean WIS (over regions and horizons) is lower;
# and 50% / 95% interval coverage of each model.
F4_FLAT <- "log_sym_wall"; F4_SEAS <- "seasonal_last3y"
cols <- c("location", "reference_date", "horizon", "model", "wis", "cov50", "cov95")
f4 <- load_scores(ILI_MAIN, models = F4_FLAT)[, ..cols]
f4 <- rbind(f4, as.data.table(arrow::read_parquet(file.path(OUT_DIR, "scores", paste0(ILI_MAIN, "_seasonal.parquet"))))[
  model == F4_SEAS & !is.na(wis), ..cols][unique(f4[, .(location, reference_date, horizon)]),
                                          on = .(location, reference_date, horizon), nomatch = 0])
stopifnot(f4[, .N, by = model][, uniqueN(N) == 1])
f4[, season := flu_season(reference_date)]
f4_pct <- function(by = NULL) {
  w <- dcast(f4[, .(wis = mean(wis)), by = c(by, "model")],
             as.formula(paste(if (length(by)) by else ".", "~ model")), value.var = "wis")
  w[, pct := 100 * (1 - get(F4_FLAT) / get(F4_SEAS))][]
}
add("fig4_flat_vs_seasonal_overall", "Figure 4 flatline vs seasonal, % WIS improvement: overall", f4_pct()$pct)
# By horizon from Figure S13 (same two models), whose forecasts are those with the target week in
# October-April (from 07_wili_long_horizons.R), so they differ slightly from the Figure 4 forecasts.
hz <- unique(tab("wili_long_horizon_summary.csv")[model == F4_FLAT & horizon <= 4])
for (h in hz$horizon)
  add(paste0("figS13_flat_vs_seasonal_h", h), paste0("Figure S13 flatline vs seasonal, % WIS improvement: horizon ", h),
      hz[horizon == h, pct_improvement_vs_seasonal_last3y])
ss <- f4_pct("season")
add("fig4_n_seasons", "Figure 4: number of seasons", nrow(ss))
add("fig4_season_mean", "Figure 4 flatline vs seasonal, % WIS improvement: mean over seasons", mean(ss$pct))
add("fig4_season_min", "Figure 4 flatline vs seasonal, % WIS improvement: smallest season", min(ss$pct))
add("fig4_season_max", "Figure 4 flatline vs seasonal, % WIS improvement: largest season", max(ss$pct))
dd <- f4_pct("reference_date")
add("fig4_n_dates", "Figure 4: number of forecast dates", nrow(dd))
add("fig4_pct_dates_flat_better", "Figure 4: % of forecast dates on which the flatline is better", 100 * mean(dd$pct > 0))
pic <- f4[, .(cov50 = 100 * mean(cov50), cov95 = 100 * mean(cov95)), by = model]
for (m in c(F4_FLAT, F4_SEAS)) {
  lab <- if (m == F4_FLAT) "flatline" else "seasonal"
  add(paste0("fig4_cov50_", lab), paste0("Figure 4: 50% interval coverage (%), ", lab), pic[model == m, cov50])
  add(paste0("fig4_cov95_", lab), paste0("Figure 4: 95% interval coverage (%), ", lab), pic[model == m, cov95])
}

# Abstract / Discussion: the best model per target (lowest mean WIS over every trend model:
# flatline or drift, any transformation and window) vs each target's standard baseline: the hub's own
# baseline submissions for COVID-19, influenza and RSV (on the forecasts the hub baseline made, as in
# variants_vs_hub_baseline.csv), and for wILI our seasonal baseline on all history (main
# specification, "seasonal") on the Figure 4 forecasts. % WIS improvement per target, then the mean,
# min and max over the four targets. Then the best wILI model vs the best seasonal baseline (the
# Figure 4 comparison, overall).
bm <- wis[, .SD[which.min(wis)], by = target][, .(target, model)]
bm <- tab("variants_vs_hub_baseline.csv")[, .(target, model, rel = rel_wis_vs_hub_baseline)][bm, on = .(target, model)]
stopifnot(bm[target == ILI_MAIN, model] == F4_FLAT)   # so the Figure 4 forecasts hold the best wILI model
seas_all <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "scores", paste0(ILI_MAIN, "_seasonal.parquet"))))[
  model == "seasonal" & !is.na(wis)][unique(f4[, .(location, reference_date, horizon)]),
                                     on = .(location, reference_date, horizon), nomatch = 0]
stopifnot(nrow(seas_all) == f4[model == F4_FLAT, .N])
bm[target == ILI_MAIN, rel := f4[model == F4_FLAT, mean(wis)] / mean(seas_all$wis)]
bm[, pct := 100 * (1 - rel)]
for (t in intersect(FIG_ORDER, bm$target))
  add(paste0("best_vs_standard_", t),
      paste0("Best model vs standard baseline (", if (t == ILI_MAIN) "seasonal, all history" else "hub baseline",
             "), % WIS improvement: ", PANEL_LABEL[t], " (", bm[target == t, model], ")"),
      bm[target == t, pct])
add("best_vs_standard_mean", "Best model vs standard baseline, % WIS improvement: mean", mean(bm$pct))
add("best_vs_standard_min", "Best model vs standard baseline, % WIS improvement: min", min(bm$pct))
add("best_vs_standard_max", "Best model vs standard baseline, % WIS improvement: max", max(bm$pct))
hb <- bm[target != ILI_MAIN, pct]   # the three hub targets only
add("best_vs_hub_baseline_mean", "Best model vs hub baseline, % WIS improvement: mean over hub targets", mean(hb))
add("best_vs_hub_baseline_min", "Best model vs hub baseline, % WIS improvement: min over hub targets", min(hb))
add("best_vs_hub_baseline_max", "Best model vs hub baseline, % WIS improvement: max over hub targets", max(hb))
add("best_wili_vs_best_seasonal", paste0("Best wILI model (", F4_FLAT, ") vs best seasonal baseline (", F4_SEAS,
                                         "), % WIS improvement"), f4_pct()$pct)

# Discussion: how the hub rankings would change if each hub had used the best model (bm above)
# instead of its own baseline submissions. Team models covering >= 50% of our forecasts (as in
# Table 1), each compared head-to-head with the baseline on the forecasts both made (ratio of mean
# WIS; < 1 = team model better). For the hub baseline and the best model: the number of team
# models that beat it, the baseline's rank among the team models plus itself, and the % of team
# models it outperforms; then the additional team models that would not beat the baseline.
for (t in intersect(FIG_ORDER, names(HUB_BASELINE))) {
  best <- bm[target == t, model]
  s <- load_scores(t, models = best)
  cells <- unique(s[, .(location, reference_date, horizon)])
  h <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "scores", paste0("hub_", t, ".parquet"))))[
    cells, on = .(location, reference_date, horizon), nomatch = 0]
  team <- h[!grepl(HUB_OWN_MODELS, model)]
  team <- team[model %in% team[, .(coverage = .N / nrow(cells)), by = model][coverage >= 0.5, model]]
  refs <- list(hub = h[model == HUB_BASELINE[[t]]], best = s)
  n_beat <- c()
  for (r in names(refs)) {
    ref <- refs[[r]][, .(location, reference_date, horizon, wis_ref = wis)]
    ratio <- merge(team[, .(model, location, reference_date, horizon, wis)], ref,
                   by = c("location", "reference_date", "horizon"))[, .(rel = mean(wis) / mean(wis_ref)), by = model]$rel
    lab <- if (r == "hub") HUB_BASELINE[[t]] else paste("best model,", best)
    n_beat[r] <- sum(ratio < 1)
    add(paste0("n_team_beat_", r, "_", t), paste0("Team models beating the ", lab, " (of ", length(ratio), "): ", PANEL_LABEL[t]),
        sum(ratio < 1))
    add(paste0("rank_", r, "_", t), paste0("Rank of the ", lab, " (of ", length(ratio) + 1, "): ", PANEL_LABEL[t]),
        1 + sum(ratio < 1))
    add(paste0("pct_team_outperformed_by_", r, "_", t), paste0("% of team models outperformed by the ", lab, ": ", PANEL_LABEL[t]),
        100 * mean(ratio > 1))
  }
  add(paste0("n_team_no_longer_beat_", t), paste0("Additional team models not beating the baseline with the best model: ", PANEL_LABEL[t]),
      n_beat[["hub"]] - n_beat[["best"]])
}

stats <- rbindlist(stats)
options(width = 150)
print(as.data.frame(stats[, .(description, value = signif(value, 4))]), right = FALSE)
fwrite(stats, file.path(fig_dir, "summary_statistics.csv"))
message("Saved summary_statistics.csv to ", fig_dir)

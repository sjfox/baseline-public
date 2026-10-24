# Summarise the scored forecasts into the tables used by the manuscript (output/tables/).
# Every percent improvement is a ratio of mean WIS over the identical set of forecasts:
# (WIS_reference - WIS_model) / WIS_reference * 100.
source("pipeline/R/setup.R")
source("pipeline/R/analysis.R")
tab_dir <- file.path(OUT_DIR, "tables")
wt <- function(x, f) fwrite(x, file.path(tab_dir, f))

MAIN_TARGETS <- intersect(MAIN_TARGETS, names(Filter(isTRUE, sapply(ALL_TARGETS, has_scores))))
SUPP_TARGETS <- intersect(SUPP_TARGETS, names(Filter(isTRUE, sapply(ALL_TARGETS, has_scores))))
TARGETS <- c(MAIN_TARGETS, SUPP_TARGETS)
FOCAL <- FOCAL_MODEL

scores <- setNames(lapply(TARGETS, load_scores), TARGETS)

# ---- Performance by variant and training window ------------------------------------------
by_window <- rbindlist(lapply(TARGETS, function(t) {
  scores[[t]][, .(wis = mean(wis), dispersion = mean(dispersion), underpred = mean(underpred),
                  overpred = mean(overpred), cov50 = mean(cov50), cov95 = mean(cov95), n = .N),
              by = .(model, variation, window)][, target := t]
}))
wt(by_window, "wis_by_window.csv")

# Breakdowns at w = 10 by horizon, location and forecast date
w10 <- paste0(rep(TRANSFORMATIONS, each = 2), c("_sym_w10", "_nonsym_w10"))
for (g in c("horizon", "location", "reference_date"))
  wt(rbindlist(lapply(TARGETS, function(t) mean_by_model(scores[[t]][model %in% w10], g)[, target := t])),
     paste0("w10_by_", g, ".csv"))

# Real-time vs final data, on identical forecast origins and locations
rt_pairs <- Filter(function(p) all(p %in% TARGETS), list(c("rsv_asof", "rsv"), c("rsv_nhsn", "rsv_nhsn_final")))
wt(rbindlist(lapply(rt_pairs, function(p) {
  keys <- unique(scores[[p[1]]][, .(location, reference_date)])
  f <- load_scores(p[2], season_only = FALSE)[keys, on = .(location, reference_date), nomatch = 0]
  r <- scores[[p[1]]][unique(f[, .(location, reference_date)]), on = .(location, reference_date), nomatch = 0]
  rbind(mean_by_model(r, c("variation", "window"))[, data := "real-time"],
        mean_by_model(f, c("variation", "window"))[, data := "final"])[, target := p[1]]
})), "realtime_vs_final.csv")

# ---- Seasonal baseline vs flatline (wILI) ---------------------------------------------
# Both models are fit to the versioned wILI data available at each origin (ili_asof).
seas_all <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "scores", paste0(ILI_MAIN, "_seasonal.parquet"))))
seasonal_compare <- function(trend_scores, sv, seas_scores = seas_all) {
  cmp <- rbind(trend_scores[model == FOCAL, .(location, reference_date, horizon, model, wis, cov50, cov95)],
               seas_scores[model == sv & !is.na(observed),
                        .(location, reference_date, horizon, model, wis, cov50, cov95)])
  cmp <- cmp[reference_date >= SEASONAL_START & month(reference_date) %in% SEASON_MONTHS]
  cmp[, n := .N, by = .(location, reference_date, horizon)]
  cmp[n == 2][, n := NULL][, season := flu_season(reference_date)][]
}
wt(seasonal_compare(scores[[ILI_MAIN]], "seasonal"), "seasonal_comparison_scores.csv")

# Seasonal-model variants (real-time wILI)
wt(rbindlist(lapply(unique(seas_all$model), function(v) {
  c2 <- seasonal_compare(scores[[ILI_MAIN]], v)
  data.table(seasonal_variant = v,
             pct_flatline_vs_seasonal = compare_models(c2, FOCAL, v)$pct,
             seasonal_wis = c2[model == v, mean(wis)], seasonal_cov95 = c2[model == v, mean(cov95)],
             pct_h1 = compare_models(c2, FOCAL, v, "horizon")[horizon == 1, pct],
             pct_h4 = compare_models(c2, FOCAL, v, "horizon")[horizon == 4, pct])
})), "seasonal_variants.csv")

# ---- Hub ensembles and submitted models ------------------------------------------------
# Hub-produced models (ensembles, hub baselines) are excluded from the set of team-submitted models.
cell_cols <- c("location", "reference_date", "horizon", "wis")
pw_cols <- c("model", cell_cols)
hub_ref <- list(); hub_pairwise <- list(); variants_vs_hb <- list()
for (t in intersect(names(HUB_BASELINE), TARGETS)) {
  f <- file.path(OUT_DIR, "scores", paste0("hub_", t, ".parquet"))
  if (!file.exists(f)) next
  ours <- scores[[t]]
  # hub forecasts restricted to exactly the cells we score (period, locations, horizons)
  h <- as.data.table(arrow::read_parquet(f))[
    unique(ours[, .(location, reference_date, horizon)]), on = .(location, reference_date, horizon), nomatch = 0]
  n_total <- uniqueN(ours[, .(location, reference_date, horizon)])
  ens <- h[model %in% HUB_ENSEMBLE[[t]]][, pref := match(model, HUB_ENSEMBLE[[t]])][
    order(pref)][, .SD[1], by = .(location, reference_date, horizon)]
  hb <- h[model == HUB_BASELINE[[t]], ..cell_cols]
  sub <- h[!grepl(HUB_OWN_MODELS, model)]
  cov <- sub[, .(coverage = .N / n_total), by = model]

  # Pairwise relative WIS against the hub baseline, as the hubs compute it (pairwise_rel_wis()):
  # pool = team models covering >= 50% of our forecasts, the hub baseline and the hub ensemble.
  pool50 <- rbind(sub[model %in% cov[coverage >= 0.5, model], ..pw_cols],
                  h[model == HUB_BASELINE[[t]], ..pw_cols],
                  ens[, c(.(model = "Hub ensemble"), .SD), .SDcols = cell_cols])
  pw <- pairwise_rel_wis(pool50, HUB_BASELINE[[t]])
  pw[, `:=`(target = t, type = fcase(model == HUB_BASELINE[[t]], "hub baseline",
                                     model == "Hub ensemble", "hub ensemble", default = "team"))]
  hub_pairwise[[t]] <- merge(pw, cov, by = "model", all.x = TRUE)[, .(target, model, type, coverage, rel_wis)]
  pw_team <- pw[type == "team", rel_wis]
  hub_ref[[t]] <- data.table(target = t, min_coverage = 0.5, n_models = length(pw_team),
    ensemble_rel_wis_pairwise = pw[model == "Hub ensemble", rel_wis],
    team_rel_wis_pairwise_q50 = quantile(pw_team, 0.5, names = FALSE),
    team_rel_wis_pairwise_q80 = quantile(pw_team, 0.8, names = FALSE),
    team_rel_wis_pairwise_geomean = exp(mean(log(pw_team))))

  # Our variants forecast every cell, so each is compared directly with the hub baseline: mean WIS
  # over the forecasts the hub baseline also made, divided by the hub baseline's mean WIS there.
  vb <- merge(ours[, ..pw_cols], hb[, .(location, reference_date, horizon, wis_hb = wis)],
              by = c("location", "reference_date", "horizon"))
  variants_vs_hb[[t]] <- vb[, .(target = t, rel_wis_vs_hub_baseline = mean(wis) / mean(wis_hb),
                                n_cells = .N, n_total = n_total), by = model]
}
wt(rbindlist(hub_ref), "hub_reference.csv")
wt(rbindlist(hub_pairwise), "hub_pairwise_rel_wis.csv")
wt(rbindlist(variants_vs_hb), "variants_vs_hub_baseline.csv")

# ---- Data revisions -------------------------------------------------------------------------
# Revision of the most recent observation at each forecast origin (the value the forecasts start
# from), for the four real-time datasets: % change from the value reported at the origin to the
# final value. Same origins and locations as the main analysis (national series excluded).
latest_revisions <- rbindlist(lapply(c(covid = "covid", flu = "flu", rsv_nhsn = "rsv_nhsn", rsv_asof = "rsv_asof"),
                                     function(t) {
  d <- load_target(t)
  tk <- d$tasks
  if (t %in% SEASONAL_TARGETS) tk <- tk[month(reference_date) %in% SEASON_MONTHS]
  if (t == "flu") tk <- tk[in_flu_seasons(reference_date)]
  tk <- tk[in_pooled(t, location)]
  x <- tk[, .(location, date = reference_date, first = vapply(series, function(y) tail(y, 1), numeric(1)))]
  x <- d$truth[, .(location, date, final = value)][x, on = .(location, date), nomatch = 0]
  x[, `:=`(target = t, revision = final - first,
           pct_revision = fifelse(first > 0, 100 * (final - first) / first, NA_real_))][]
}))
wt(latest_revisions, "data_revisions_latest.csv")

message("Tables written to ", tab_dir)

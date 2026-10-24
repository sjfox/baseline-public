# The Figure 3 tables of 05_analysis.R with every forecast scored on the log scale (wis_log: WIS of
# log(x + 1) quantiles against log(x + 1) of the observation; R/scoring.R), for our variants, the hub
# baseline and the team models alike.
# Writes: output/tables/wis_by_window_log_scale.csv, variants_vs_hub_baseline_log_scale.csv,
#         hub_reference_log_scale.csv
source("pipeline/R/setup.R")
source("pipeline/R/analysis.R")
wt <- function(x, f) fwrite(x, file.path(OUT_DIR, "tables", f))
TARGETS <- intersect(MAIN_TARGETS, names(Filter(isTRUE, sapply(MAIN_TARGETS, has_scores))))
scores <- setNames(lapply(TARGETS, function(t) load_scores(t)[, wis := wis_log]), TARGETS)

wt(rbindlist(lapply(TARGETS, function(t)
  scores[[t]][, .(wis = mean(wis), n = .N), by = .(model, variation, window)][, target := t])),
  "wis_by_window_log_scale.csv")

cell_cols <- c("location", "reference_date", "horizon", "wis")
pw_cols <- c("model", cell_cols)
hub_ref <- list(); variants_vs_hb <- list()
for (t in intersect(names(HUB_BASELINE), TARGETS)) {
  ours <- scores[[t]]
  h <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "scores", paste0("hub_", t, ".parquet"))))[
    unique(ours[, .(location, reference_date, horizon)]), on = .(location, reference_date, horizon), nomatch = 0]
  h[, wis := wis_log]
  n_total <- uniqueN(ours[, .(location, reference_date, horizon)])
  ens <- h[model %in% HUB_ENSEMBLE[[t]]][, pref := match(model, HUB_ENSEMBLE[[t]])][
    order(pref)][, .SD[1], by = .(location, reference_date, horizon)]
  hb <- h[model == HUB_BASELINE[[t]], ..cell_cols]
  sub <- h[!grepl(HUB_OWN_MODELS, model)]
  cov <- sub[, .(coverage = .N / n_total), by = model]
  pool50 <- rbind(sub[model %in% cov[coverage >= 0.5, model], ..pw_cols],
                  h[model == HUB_BASELINE[[t]], ..pw_cols],
                  ens[, c(.(model = "Hub ensemble"), .SD), .SDcols = cell_cols])
  pw <- pairwise_rel_wis(pool50, HUB_BASELINE[[t]])
  pw_team <- pw[!model %in% c(HUB_BASELINE[[t]], "Hub ensemble"), rel_wis]
  hub_ref[[t]] <- data.table(target = t, min_coverage = 0.5, n_models = length(pw_team),
    ensemble_rel_wis_pairwise = pw[model == "Hub ensemble", rel_wis],
    team_rel_wis_pairwise_q50 = quantile(pw_team, 0.5, names = FALSE),
    team_rel_wis_pairwise_q80 = quantile(pw_team, 0.8, names = FALSE),
    team_rel_wis_pairwise_geomean = exp(mean(log(pw_team))))
  vb <- merge(ours[, ..pw_cols], hb[, .(location, reference_date, horizon, wis_hb = wis)],
              by = c("location", "reference_date", "horizon"))
  variants_vs_hb[[t]] <- vb[, .(target = t, rel_wis_vs_hub_baseline = mean(wis) / mean(wis_hb),
                                n_cells = .N, n_total = n_total), by = model]
}
wt(rbindlist(hub_ref), "hub_reference_log_scale.csv")
wt(rbindlist(variants_vs_hb), "variants_vs_hub_baseline_log_scale.csv")

# Helpers for loading scores and comparing models.
# Conventions:
#   * percent improvement of model A over B = (WIS_B - WIS_A) / WIS_B * 100, using mean WIS
#     over the identical set of forecasts (ratio of means).
#   * "variation": flatline/drift x transformation;  "window": number of differences or "all".

library(data.table)

NATIONAL <- c("US", "nat")

VARIATION_LABELS <- c(none_sym = "Flatline", none_nonsym = "Drift",
                      sqrt_sym = "Flatline (transformed)", sqrt_nonsym = "Drift (transformed)",
                      log_sym = "Flatline (log)", log_nonsym = "Drift (log)")

# Load scores for a target, restricted to the analysis set, with complete cases across models.
SEASONAL_TARGETS <- c("rsv", "rsv_asof", "ili_asof")   # evaluated Oct-Apr only
has_scores <- function(target) file.exists(file.path(OUT_DIR, "scores", paste0(target, ".parquet")))

# The national series is dropped for targets in NATIONAL_EXCLUDED_TARGETS.
load_scores <- function(target, models = NULL, season_only = TRUE) {
  s <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "scores", paste0(target, ".parquet"))))
  if (target %in% NATIONAL_EXCLUDED_TARGETS) s <- s[!location %in% NATIONAL]
  if (season_only && target %in% SEASONAL_TARGETS) s <- s[month(reference_date) %in% SEASON_MONTHS]
  if (target == "covid") s <- s[between(snapshot, PERIODS$covid[1], PERIODS$covid[2])]
  if (target == "ili_asof") s <- s[reference_date >= SEASONAL_START]
  if (target == "flu") s <- s[in_flu_seasons(reference_date)]
  if (!is.null(models)) s <- s[model %in% models]
  s <- s[!is.na(observed)]
  n_models <- uniqueN(s$model)
  s[, n_m := .N, by = .(location, reference_date, horizon)]
  dropped <- s[n_m < n_models, uniqueN(paste(location, reference_date, horizon))]
  if (dropped > 0) message(target, ": dropping ", dropped, " incomplete forecast cells")
  s <- s[n_m == n_models][, n_m := NULL]
  s[, variation := VARIATION_LABELS[sub("_w.*$", "", model)]]
  s[, window := sub("^.*_w", "", model)]
  s[]
}

pct_improve <- function(wis_model, wis_reference) (wis_reference - wis_model) / wis_reference * 100

mean_by_model <- function(s, by = NULL) {
  s[, .(wis = mean(wis), cov50 = mean(cov50), cov95 = mean(cov95), n = .N), by = c("model", by)]
}

# % improvement of `model` over `reference` (ratio of mean WIS), optionally by group.
compare_models <- function(s, mdl, ref, by = NULL) {
  m <- mean_by_model(s[model %in% c(mdl, ref)], by)
  w <- dcast(m, as.formula(paste(if (length(by)) paste(by, collapse = "+") else ".", "~ model")),
             value.var = "wis")
  w[, pct := pct_improve(get(mdl), get(ref))]
  w[]
}

# TRUE for forecast cells that the main analysis pools (drops excluded national series).
in_pooled <- function(target, location) !(target %in% NATIONAL_EXCLUDED_TARGETS & location %in% NATIONAL)

flu_season <- function(d) ifelse(month(d) >= 10, year(d), year(d) - 1)

# Pairwise relative WIS of every model in `s` (columns model, location, reference_date, horizon,
# wis), scaled so that `baseline` = 1: the method of the US forecast hubs (Cramer et al. 2022),
# computed with scoringutils::add_relative_skill, which hubEvals::score_model_out also uses. For
# each pair of models, the ratio of mean WIS on the forecasts both made; each model's relative
# skill is the geometric mean of its ratios against all other models, divided by the baseline's.
pairwise_rel_wis <- function(s, baseline) {
  x <- s[, .(model, location, reference_date, horizon, wis)]
  class(x) <- c("scores", class(x))                 # a scoringutils scores object (as score() returns)
  setattr(x, "metrics", "wis")
  r <- suppressWarnings(scoringutils::add_relative_skill(x, compare = "model", metric = "wis",
                                                         baseline = baseline))
  unique(as.data.table(r)[, .(model, rel_wis = wis_scaled_relative_skill)])
}

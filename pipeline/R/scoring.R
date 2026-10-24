# Weighted interval score and interval coverage for quantile forecasts.
#
# Definition (Bracher et al. 2021; scoringutils defaults):
#   WIS = [ 0.5 * |y - median| + sum_k (alpha_k / 2) * IS_alpha_k ] / (K + 0.5)
#   IS_alpha = (u - l) + (2/alpha)(l - y) 1{y < l} + (2/alpha)(y - u) 1{y > u}
# Coverage of a central interval counts y on the bounds as covered.

# q: matrix [n_forecasts x n_quantiles] (columns ordered as `quantiles`); y: length n.
score_quantiles <- function(q, y, quantiles = QUANTILES) {
  stopifnot(ncol(q) == length(quantiles), nrow(q) == length(y))
  med_col <- which(abs(quantiles - 0.5) < 1e-9)
  lower   <- which(quantiles < 0.5 - 1e-9)
  upper   <- rev(which(quantiles > 0.5 + 1e-9))
  alphas  <- 2 * quantiles[lower]
  K <- length(alphas)

  # underprediction: observation above the forecast; overprediction: below it.
  med   <- q[, med_col]
  disp  <- rep(0, length(y))
  under <- 0.5 * pmax(0, y - med)      # median contributes 0.5 * |y - m|
  over  <- 0.5 * pmax(0, med - y)
  cov <- list()
  for (k in seq_len(K)) {
    a <- alphas[k]; l <- q[, lower[k]]; u <- q[, upper[k]]
    disp  <- disp  + (a / 2) * (u - l)
    under <- under + pmax(0, y - u)    # (a/2) * (2/a) * (y - u)
    over  <- over  + pmax(0, l - y)
    lvl <- round(100 * (1 - a))
    if (lvl %in% c(50, 95)) cov[[paste0("cov", lvl)]] <- as.integer(y >= l & y <= u)
  }
  denom <- K + 0.5
  data.frame(
    wis          = (disp + under + over) / denom,
    dispersion   = disp / denom,
    underpred    = under / denom,
    overpred     = over / denom,
    abs_error    = abs(y - med),
    cov50        = cov$cov50,
    cov95        = cov$cov95
  )
}

# WIS on the log scale (Bosse et al. 2023): the same score applied to log(x + 1) of the forecast
# quantiles and of the observation. Stored as `wis_log` beside `wis` for our models and hub models.
wis_log_scale <- function(q, y, quantiles = QUANTILES)
  score_quantiles(log1p(pmax(q, 0)), log1p(pmax(y, 0)), quantiles)$wis

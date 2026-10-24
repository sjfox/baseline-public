# Trend baseline ("quantile_baseline") forecasts.
#
# This is a vectorised re-implementation of simplets::fit_simple_ts(model = "quantile_baseline")
# + predict(origin = "obs", force_nonneg = TRUE) with d = D = 0. Given the same RNG state it
# returns bit-identical trajectories to simplets but avoids
# simplets' per-trajectory R loop, and only materialises the horizons that are scored.
#
# Algorithm (per series, on the transformed scale):
#   D      = last `window` first differences (NAs dropped); if flatline, D <- c(D, -D)
#   s      = nsim evenly spaced quantiles of D (type 7)
#   h = 1  : origin + s
#   h > 1  : add a fresh random permutation of s; for flatline, recentre so median == origin
#   values are truncated at 0 on the transformed scale, then back-transformed.

transform_fwd <- function(y, transformation, offset) {
  switch(transformation, none = y, sqrt = sqrt(y + offset), log = log(y + offset))
}
transform_inv <- function(x, transformation, offset) {
  switch(transformation, none = x, sqrt = x^2 - offset, log = exp(x) - offset)
}

# Returns an nsim x length(keep) matrix of trajectories at horizons `keep`.
simulate_quantile_baseline <- function(ty, symmetrize, window, nsim, keep) {
  n  <- length(ty)
  ws <- if (is.infinite(window) || window >= n) n - 1 else window
  d  <- utils::tail(diff(ty), ws)
  d  <- d[!is.na(d)]
  if (length(d) == 0) return(NULL)
  if (symmetrize) d <- c(d, -d)
  origin <- ty[n]
  s   <- stats::quantile(d, probs = seq(0, 1, length.out = nsim), names = FALSE)
  raw <- origin + s
  out <- matrix(NA_real_, nsim, length(keep))
  k <- 1L
  for (h in seq_len(max(keep))) {
    if (h > 1L) {
      s   <- s[sample.int(nsim)]
      raw <- raw + s
    }
    if (h == keep[k]) {
      v <- if (symmetrize && h > 1L) raw - (stats::median(raw) - origin) else raw
      out[, k] <- pmax(0, v)
      k <- k + 1L
    }
  }
  out
}

# Forecast quantiles for one series and one model configuration.
#   y           : numeric series ending at the forecast origin (no trailing NA)
#   count_data  : TRUE -> quantiles rounded up to integers (count targets)
# Returns a matrix [length(keep) x length(quantiles)].
forecast_quantiles <- function(y, transformation, symmetrize, window, keep,
                               nsim = NSIM, quantiles = QUANTILES,
                               offset = TRANSFORM_OFFSET, count_data = TRUE) {
  ty   <- transform_fwd(y, transformation, offset)
  sims <- simulate_quantile_baseline(ty, symmetrize, window, nsim, keep)
  if (is.null(sims)) return(NULL)
  sims <- transform_inv(sims, transformation, offset)
  q <- matrix(0, length(keep), length(quantiles))
  for (k in seq_along(keep)) {
    qk <- stats::quantile(sims[, k], probs = quantiles, names = FALSE)
    if (count_data) qk <- ceiling(qk)
    q[k, ] <- cummax(pmax(0, qk))      # non-negative, non-decreasing
  }
  q
}

# ---- Batched version: all windows for one series at once --------------------------------
# Same algorithm, but every window shares the same random permutation at each horizon
# (common random numbers). Each window's predictive distribution is identical in law to
# simplets; Monte Carlo noise is positively correlated across windows, which makes window
# comparisons more precise. Returns array [length(keep), length(quantiles), length(windows)].
forecast_quantiles_batch <- function(y, transformation, symmetrize, windows, keep,
                                     nsim = NSIM, quantiles = QUANTILES,
                                     offset = TRANSFORM_OFFSET, count_data = TRUE) {
  ty <- transform_fwd(y, transformation, offset)
  n  <- length(ty)
  dy <- diff(ty)
  probs <- seq(0, 1, length.out = nsim)
  ok <- logical(length(windows))
  S <- matrix(NA_real_, nsim, length(windows))
  for (j in seq_along(windows)) {
    w <- windows[j]
    ws <- if (is.infinite(w) || w >= n) n - 1 else w
    d <- utils::tail(dy, ws); d <- d[!is.na(d)]
    if (length(d) == 0) next
    if (symmetrize) d <- c(d, -d)
    S[, j] <- stats::quantile(d, probs = probs, names = FALSE)
    ok[j] <- TRUE
  }
  origin <- ty[n]
  raw <- origin + S
  out <- array(NA_real_, c(length(keep), length(quantiles), length(windows)))
  k <- 1L
  for (h in seq_len(max(keep))) {
    if (h > 1L) {
      S   <- S[sample.int(nsim), , drop = FALSE]
      raw <- raw + S
    }
    if (h == keep[k]) {
      v <- raw
      if (symmetrize && h > 1L)
        v <- v - rep(matrixStats::colMedians(raw) - origin, each = nsim)
      v[] <- transform_inv(pmax(0, v), transformation, offset)
      q <- matrixStats::colQuantiles(v, probs = quantiles, type = 7L, drop = FALSE)  # windows x quantiles
      if (count_data) q <- ceiling(q)
      q[] <- pmax(0, q)
      q <- t(apply(q, 1, cummax))
      out[k, , ] <- t(q)
      k <- k + 1L
    }
  }
  out[, , !ok] <- NA
  out
}

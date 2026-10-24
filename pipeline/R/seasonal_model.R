# Seasonal baseline for wILI (FluSight 2014-2018 style; Ray & Reich 2018).
#
#   log(wILI_t) = b0 + s(season_week_t) + e_t,  s = cyclic cubic spline, e ~ N(0, sigma^2)
# fit separately per region to ALL data available up to the forecast date (pooled seasons).
# Predictive distribution: draw coefficients from N(beta_hat, V_beta), add residual noise on
# the log scale, exponentiate, take empirical quantiles.

library(mgcv)

# Season week = weeks since MMWR week 40 of the season's starting year (1..52 or 53).
season_week <- function(dates) {
  mm <- MMWRweek::MMWRweek(dates)
  season <- ifelse(mm$MMWRweek >= 40, mm$MMWRyear, mm$MMWRyear - 1)
  start <- MMWRweek::MMWRweek2Date(season, rep(40L, length(season)))
  as.integer(dates - start) %/% 7L + 1L
}

# transform  : "log" (default; log(max(wILI, floor))) or "none" (Gaussian on the wILI scale)
# n_years    : if set, train only on the most recent n_years of data before the forecast date
seasonal_forecast <- function(train, target_dates, n_sim = 1000, quantiles = QUANTILES,
                              floor_value = 0.001, transform = "log",
                              n_years = NULL) {
  train <- train[!is.na(value)]
  if (!is.null(n_years)) train <- train[date > min(target_dates) - 7 - round(365.25 * n_years)]
  train[, sw := season_week(date)]
  train[, ly := if (transform == "log") log(pmax(value, floor_value)) else value]
  fm <- gam(ly ~ s(sw, bs = "cc"), data = train, knots = list(sw = c(0.5, 53.5)))
  beta <- coef(fm); Vb <- vcov(fm)
  br <- t(chol(Vb)) %*% matrix(rnorm(n_sim * length(beta)), length(beta), n_sim) + beta
  Xp <- predict(fm, newdata = data.frame(sw = season_week(target_dates)), type = "lpmatrix")
  lp <- Xp %*% br
  sims <- matrix(rnorm(length(lp), mean = lp, sd = sqrt(fm$sig2)), nrow = nrow(lp))
  if (transform == "log") sims <- exp(sims) else sims[] <- pmax(0, sims)
  q <- t(apply(sims, 1, stats::quantile, probs = quantiles, names = FALSE))
  t(apply(q, 1, cummax))
}

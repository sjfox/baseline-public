# Plotting functions for performance against training window: relative WIS (Figures 3, S6 and S8) and
# interval coverage (Figures S9 and S10). Sourced after _setup.R.
LOG_Y      <- TRUE                                     # log-scale y-axis
FONT_SIZE  <- 14                                       # base text size (the default figure theme uses 11)
REF_SHOW   <- c("q50", "q80")                          # reference lines drawn (any of Ensemble, Average, q50, q80)
REF_LTY    <- "4313"                                   # reference lines: black dash-dot (unlike "22" / "13")
REF_LWD    <- 0.8                                      # reference line width
ONE_LINE   <- list(colour = "black", alpha = 0.35, linewidth = 0.5)   # y = 1: same as the standard baseline
SHAPES     <- c(Raw = 21, `Square root` = 22, Log = 24) # "all data" points: circle, square, triangle
POINT_ALPHA <- 0.6                                     # filled "all data" points, see-through where they overlap
REF_LABELS <- c(Ensemble = "Hub ensemble", Average = "Average submitted model", q50 = "50%", q80 = "80%")
REF_FRAC   <- c(Ensemble = 0, Average = 0, q50 = 0, q80 = 0)      # label positions across the panel (0 = left)
REF_HJUST  <- c(Ensemble = 0, Average = 0, q50 = 0, q80 = 0)

# wILI has no hub: its standard baseline is the seasonal model (main specification), scored on the
# same forecasts (the cells in seasonal_comparison_scores.csv). Mean WIS (or log-scale WIS) of the
# seasonal baseline over those forecasts.
seasonal_baseline_wis <- function(log_scale = FALSE) {
  cells <- unique(tab("seasonal_comparison_scores.csv")[, .(location, reference_date = as.Date(reference_date), horizon)])
  s <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "scores", paste0(ILI_MAIN, "_seasonal.parquet"))))[
    model == "seasonal"][cells, on = .(location, reference_date, horizon), nomatch = 0]
  stopifnot(nrow(s) == nrow(cells))
  mean(if (log_scale) s$wis_log else s$wis)
}

# suffix: "" for the main tables, "_log_scale" for the log-scale scoring tables (08_log_scale_scoring.R)
# targets: which panels to draw (default: all four main targets). Panels keep their Figure 3 letters
# (A COVID-19 ... D RSV), so a single panel drawn on its own is labelled as in the full figure.
plot_fig3 <- function(exclude_log = FALSE, suffix = "", targets = FIG_ORDER) {
  set <- intersect(targets, MAIN)
  lv <- panel_levels(intersect(FIG_ORDER, MAIN))[set]
  # Hub reference lines: pairwise relative WIS against the hub baseline
  hr <- tab(paste0("hub_reference", suffix, ".csv"))[min_coverage == 0.5 & target %in% set,
    .(target, Ensemble = ensemble_rel_wis_pairwise, Average = team_rel_wis_pairwise_geomean,
      q50 = team_rel_wis_pairwise_q50, q80 = team_rel_wis_pairwise_q80)]
  rel_hb <- tab(paste0("variants_vs_hub_baseline", suffix, ".csv"))[, .(target, model, rel_wis_vs_hub_baseline)]
  # every panel relative to its target's standard baseline: the hub baseline, or for wILI the seasonal one
  if (ILI_MAIN %in% set) {
    seas <- seasonal_baseline_wis(log_scale = suffix == "_log_scale")
    rel_hb <- rbind(rel_hb[target != ILI_MAIN],
                    tab(paste0("wis_by_window", suffix, ".csv"))[target == ILI_MAIN,
                                             .(target, model, rel_wis_vs_hub_baseline = wis / seas)])
  }
  d <- label_rows(window_x(tab(paste0("wis_by_window", suffix, ".csv"))[target %in% set]), set)
  d[, panel := factor(lv[target], levels = lv)]
  if (exclude_log) d <- d[transform != "Log"]
  d <- rel_hb[d, on = .(target, model)]
  d[, y := rel_wis_vs_hub_baseline]
  # Log-transformed models can be extreme (small counts with reporting spikes): show them only
  # within 1.5x the range of the other models in each panel
  d[, cap := 1.5 * max(y[transform != "Log"]), by = target]
  n_capped <- d[y > cap, .N]
  d[y > cap, y := NA]

  ref <- melt(hr, id.vars = "target", variable.name = "ref", value.name = "y")
  ref[, label := REF_LABELS[as.character(ref)]]
  ref <- ref[ref %in% REF_SHOW]
  xr <- d[window != "all", .(xmin = min(x), xmax = max(x)), by = target]
  ref <- xr[ref, on = "target"]
  ref[, `:=`(lx = exp(log(xmin) + REF_FRAC[as.character(ref)] * (log(xmax) - log(xmin))),
             hj = REF_HJUST[as.character(ref)])]
  # labels at position 0 sit at the left edge of the panel, next to the y-axis (x = 0 is -Inf on
  # the log axis, which ggplot draws at the panel edge without widening the axis)
  ref[REF_FRAC[as.character(ref)] == 0, lx := 0]
  ref[, panel := factor(lv[target], levels = lv)]

  p <- ggplot(d[window != "all"], aes(x, y, colour = family)) +
    geom_hline(yintercept = 1, colour = ONE_LINE$colour, alpha = ONE_LINE$alpha, linewidth = ONE_LINE$linewidth) +
    geom_hline(data = ref, aes(yintercept = y), colour = "black", linetype = REF_LTY, linewidth = REF_LWD) +
    geom_label(data = ref, aes(x = lx, y = y, label = label, hjust = hj), inherit.aes = FALSE,
               vjust = -0.15, size = FONT_SIZE / 3.6, colour = "grey25", fill = "white", label.size = 0,
               label.padding = unit(0.08, "lines")) +
    geom_line(aes(linetype = transform), linewidth = 0.6) +
    geom_point(data = d[window == "all"], aes(shape = transform, fill = family), size = 3,
               alpha = POINT_ALPHA, stroke = 0.9) +
    facet_wrap(~panel, scales = "free", ncol = 2) +
    scale_family() + scale_family("fill") + scale_transform() +
    scale_shape_manual(values = SHAPES, name = NULL) +
    scale_x_continuous(trans = "log10",
                       breaks = function(l) { b <- c(5, 10, 20, 50, 100, 200, 360); b[b <= l[2]] }) +
    labs(x = XLAB_WINDOW,
         y = if (suffix == "_log_scale") "Log-scale WIS relative to the standard baseline"
             else "WIS relative to the standard baseline",
         caption = if (n_capped) "Log-transformed models are not drawn where their WIS exceeds 1.5 times the largest WIS of the other models in the panel.") +
    base_theme(FONT_SIZE) +
    theme(strip.text = element_text(size = FONT_SIZE + 1, face = "bold", hjust = 0),
          legend.position = "right", legend.justification = "center",
          legend.key.width = unit(2.4, "lines"),
          plot.caption = element_text(hjust = 0, size = FONT_SIZE - 3, colour = "grey30"),
          plot.caption.position = "plot") +
    guides(colour = guide_legend(order = 1, override.aes = list(linewidth = 1.2, shape = NA)),
           fill = "none",
           linetype = guide_legend(order = 2, override.aes = list(linewidth = 1.1)),
           shape = guide_legend(order = 2, override.aes = list(fill = "grey60", alpha = POINT_ALPHA)))
  if (LOG_Y) p <- p + scale_y_continuous(trans = "log10", breaks = scales::breaks_pretty(n = 6))
  p
}

# Interval coverage against training window (Figures S9, S10), styled as Figure 3: same panel order and
# labels, filled "all data" points; grey line = nominal coverage. exclude_log = TRUE drops the log models.
plot_coverage <- function(metric, nominal, ylab, exclude_log = FALSE) {
  set <- intersect(FIG_ORDER, MAIN)
  d <- label_rows(window_x(tab("wis_by_window.csv")[target %in% set]), set)
  if (exclude_log) d <- d[transform != "Log"]
  d[, y := 100 * get(metric)]
  ggplot(d[window != "all"], aes(x, y, colour = family)) +
    geom_hline(yintercept = nominal, colour = ONE_LINE$colour, alpha = ONE_LINE$alpha, linewidth = ONE_LINE$linewidth) +
    geom_line(aes(linetype = transform), linewidth = 0.6) +
    geom_point(data = d[window == "all"], aes(shape = transform, fill = family), size = 3,
               alpha = POINT_ALPHA, stroke = 0.9) +
    facet_wrap(~panel, scales = "free", ncol = 2) +
    scale_family() + scale_family("fill") + scale_transform() +
    scale_shape_manual(values = SHAPES, name = NULL) +
    scale_x_continuous(trans = "log10",
                       breaks = function(l) { b <- c(5, 10, 20, 50, 100, 200, 360); b[b <= l[2]] }) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(x = XLAB_WINDOW, y = ylab) +
    base_theme(FONT_SIZE) +
    theme(strip.text = element_text(size = FONT_SIZE + 1, face = "bold", hjust = 0),
          legend.position = "right", legend.justification = "center",
          legend.key.width = unit(2.4, "lines")) +
    guides(colour = guide_legend(order = 1, override.aes = list(linewidth = 1.2, shape = NA)),
           fill = "none",
           linetype = guide_legend(order = 2, override.aes = list(linewidth = 1.1)),
           shape = guide_legend(order = 2, override.aes = list(fill = "grey60", alpha = POINT_ALPHA)))
}

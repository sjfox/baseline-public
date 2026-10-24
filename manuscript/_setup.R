# Shared setup for the manuscript figure and table scripts: packages, paths, colours, theme and
# helper functions. Scripts read the pipeline outputs (pipeline/output/) and write to
# manuscript/output/, named by their manuscript number in manuscript/items.csv.
# Run from the project root.
source("pipeline/R/setup.R")
source("pipeline/R/analysis.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })

tab <- function(f) fread(file.path(OUT_DIR, "tables", f))
ITEMS <- fread("manuscript/items.csv")
fig_dir <- file.path("manuscript", "output")
dir.create(fig_dir, showWarnings = FALSE)
# Writes manuscript/output/<number>.png and .pdf, where <number> is the manuscript number of
# `script` in items.csv (e.g. "Figure_3"). Width and height in inches.
save_fig <- function(p, script, w, h) {
  i <- match(script, ITEMS$script)
  if (is.na(i)) stop(script, " is not listed in manuscript/items.csv")
  name <- gsub(" ", "_", ITEMS$item[i])
  files <- file.path(fig_dir, paste0(name, c(".png", ".pdf")))
  ggsave(files[1], p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(files[2], p, width = w, height = h, bg = "white")
  message("Saved ", name, " (", w, " x ", h, " in) to ", fig_dir)
  invisible(files)
}

# Writes manuscript/output/<number>.csv for a table script listed in items.csv (e.g. "Table_1").
save_table <- function(x, script) {
  i <- match(script, ITEMS$script)
  if (is.na(i)) stop(script, " is not listed in manuscript/items.csv")
  f <- file.path(fig_dir, paste0(gsub(" ", "_", ITEMS$item[i]), ".csv"))
  fwrite(x, f)
  message("Saved ", basename(f), " to ", fig_dir)
  invisible(f)
}

MAIN <- MAIN_TARGETS
PANEL_LABEL <- c(covid = "COVID-19", flu = "Influenza", rsv_nhsn = "RSV", ili_asof = "wILI",
                 rsv_asof = "RSV-NET (real-time data)", rsv = "RSV-NET (final data)",
                 rsv_nhsn_final = "RSV (NHSN, final data)")
panel_levels <- function(set) setNames(paste0(LETTERS[seq_along(set)], ". ", PANEL_LABEL[set]), set)

# Panel order of the main figures (Figures 2 and 3): COVID-19, influenza / wILI, RSV
FIG_ORDER <- c("covid", "flu", "ili_asof", "rsv_nhsn")
# x-axis title of every training-window figure
XLAB_WINDOW <- expression(paste("Training window width (", italic(w), ")"))

# Colours: flatline / drift / seasonal; diverging pair for B vs A.
COL <- c(Flatline = "#2a78d6", Drift = "#eb6834", Seasonal = "#d1343a")
DIVERGING <- c(low = "#eb6834", mid = "#e8e7e1", high = "#2a78d6")
LT <- c(Raw = "22", `Square root` = "solid", Log = "13")
# Base theme for every figure: cowplot, with major x and y gridlines behind the data.
# Facet titles bold and left-aligned without a grey box; legends centred at the bottom.
# A figure that needs bigger text adds base_theme(<size>) before its own theme() tweaks.
base_theme <- function(font_size = 11) {
  cowplot::theme_cowplot(font_size = font_size) +
    cowplot::background_grid(major = "xy", minor = "none") +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold", hjust = 0),
          legend.position = "bottom", legend.justification = "center",
          plot.title.position = "plot")
}
theme_set(base_theme(11))
scale_family <- function(aes = "colour") scale_discrete_manual(aes, values = COL[c("Flatline", "Drift")], name = NULL)
scale_transform <- function() scale_linetype_manual(values = LT, name = NULL)

# Location labels: two-letter state abbreviations; wILI regions as "National", "HHS 1" ... "HHS 10"
loc_label <- function(location) {
  locs <- fread(file.path(DATA_DIR, "locations.csv"), colClasses = "character")
  out <- locs$abbreviation[match(location, locs$location)]
  out[location == "nat"] <- "National"
  hhs <- grepl("^hhs", location)
  out[hhs] <- paste("HHS", sub("^hhs", "", location[hhs]))
  out
}

# family (Flatline/Drift), transformation label, and panel for each row
label_rows <- function(d, set) {
  d[, family := ifelse(grepl("_sym_", model), "Flatline", "Drift")]
  d[, transform := c(none = "Raw", sqrt = "Square root", log = "Log")[sub("_.*$", "", model)]]
  lv <- panel_levels(set)
  d[, panel := factor(lv[target], levels = lv)]
  d[]
}
# Numeric x for training windows; "all" drawn just right of the largest window.
window_x <- function(d) {
  d[, x := suppressWarnings(as.numeric(window))]
  d[, x := fifelse(window == "all", max(x, na.rm = TRUE) * 1.12, x), by = target]
  d[]
}

# ==============================================================
# Division-level choropleth maps, BDHS 2022
# Figure 3 (access barrier), Figure 4 (depression), Figure 5 (anxiety)
#
# Cartographic choices, and why:
#  - Classed (binned) fill, not a continuous ramp. Readers compare
#    eight administrative units, not a smooth surface, so discrete
#    classes with stated breaks are read more accurately.
#  - ColorBrewer sequential schemes. They are ordered by lightness,
#    print in greyscale, and are safe for colour vision deficiency.
#  - Figures 4 and 5 share one palette AND one set of breaks, so the
#    two outcomes can be compared directly between the maps. With
#    separate scales a division can look darker on one map only
#    because that map was rescaled.
#  - Figure 3 uses a different hue because it shows a different
#    construct, and its values are on a different range.
#  - The value is printed under each division name. A choropleth
#    shows pattern; the number gives the reader the estimate.
#  - Graticule labels removed. Degrees of latitude carry no
#    information about an administrative unit; a scale bar and a
#    north arrow do.
# ==============================================================

need <- c("sf", "ggplot2", "dplyr", "tibble", "ggspatial", "geodata")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) stop("Install first: install.packages(c(",
                       paste0('"', miss, '"', collapse = ", "), "))")

library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)   # scale bar and north arrow

# --------------------------------------------------------------
# 1. Division boundaries
# --------------------------------------------------------------
bgd <- geodata::gadm("BGD", level = 1, path = tempdir()) |> sf::st_as_sf()
name_col <- "NAME_1"          # GADM. Use "ADM1_EN" for the HDX/BBS shapefile.

# --------------------------------------------------------------
# 2. Survey-weighted division estimates (match Table 3)
# --------------------------------------------------------------
est <- tibble::tribble(
  ~gadm_name,    ~label,        ~distance, ~depression, ~anxiety,
  "Barisal",     "Barishal",         48.7,        4.71,     4.13,
  "Chittagong",  "Chattogram",       53.5,        4.19,     4.96,
  "Dhaka",       "Dhaka",            41.2,        4.16,     3.44,
  "Khulna",      "Khulna",           40.9,        6.54,     5.78,
  "Mymensingh",  "Mymensingh",       50.5,        3.76,     3.19,
  "Rajshahi",    "Rajshahi",         49.1,        3.74,     3.07,
  "Rangpur",     "Rangpur",          41.2,        7.60,     5.73,
  "Sylhet",      "Sylhet",           48.6,        6.46,     5.72
)

bgd$join_name <- bgd[[name_col]]
map_df <- left_join(bgd, est, by = c("join_name" = "gadm_name"))
missing <- map_df$join_name[is.na(map_df$label)]
if (length(missing) > 0) {
  stop("Boundary names not matched: ", paste(missing, collapse = ", "))
}

# --------------------------------------------------------------
# Label positions
# --------------------------------------------------------------
# st_point_on_surface puts the anchor wherever the polygon allows,
# which for Barishal and Chattogram lands on or beyond a boundary.
# Explicit longitude and latitude are used instead, one pair per
# division, chosen to sit well inside the polygon. If a label still
# sits awkwardly after a boundary file change, edit the two numbers
# for that division here and nothing else.
lab <- data.frame(
  label = c("Rangpur", "Rajshahi", "Mymensingh", "Sylhet",
            "Dhaka", "Khulna", "Barishal", "Chattogram"),
  X = c(89.25, 88.95, 90.45, 91.70,
        90.05, 89.20, 90.30, 92.05),
  Y = c(25.75, 24.50, 24.95, 24.85,
        23.95, 22.95, 22.45, 22.20),
  stringsAsFactors = FALSE
)
lab <- dplyr::left_join(lab, sf::st_drop_geometry(map_df)[, c("label", "distance", "depression", "anxiety")],
                        by = "label")

# --------------------------------------------------------------
# 3. One styling function
# --------------------------------------------------------------
draw_map <- function(var, legend_title, palette, breaks, digits = 1) {
  lab$value <- lab[[var]]
  ggplot(map_df) +
    geom_sf(aes(fill = .data[[var]]), colour = "grey25", linewidth = 0.3) +
    geom_text(data = lab,
              aes(X, Y, label = paste0(label, "\n", formatC(value, format = "f", digits = digits))),
              size = 2.9, lineheight = 0.95, colour = "black", family = "sans") +
    scale_fill_fermenter(palette = palette, direction = 1, breaks = breaks,
                         name = legend_title,
                         guide = guide_colorsteps(barheight = unit(45, "mm"),
                                                  barwidth  = unit(4.5, "mm"),
                                                  show.limits = TRUE,
                                                  ticks = FALSE)) +
    annotation_scale(location = "bl", width_hint = 0.28, height = unit(1.6, "mm"),
                     text_cex = 0.6, line_width = 0.4) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           height = unit(8, "mm"), width = unit(8, "mm"),
                           style = north_arrow_minimal(line_width = 0.6, text_size = 7)) +
    coord_sf(expand = TRUE) +
    theme_void(base_size = 10) +
    theme(
      panel.border    = element_rect(colour = "grey40", fill = NA, linewidth = 0.4),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      legend.title     = element_text(size = 8.5, lineheight = 1.05),
      legend.text      = element_text(size = 8),
      plot.margin      = margin(4, 4, 4, 4)
    )
}

# Figures 4 and 5 deliberately share palette and breaks
outcome_breaks <- c(3, 4, 5, 6, 7)

fig3 <- draw_map("distance",   "Distance a big\nproblem (%)",
                 palette = "YlOrBr", breaks = c(42, 45, 48, 51))
fig4 <- draw_map("depression", "Depression\nprevalence (%)",
                 palette = "PuBu",   breaks = outcome_breaks, digits = 2)
fig5 <- draw_map("anxiety",    "Anxiety\nprevalence (%)",
                 palette = "PuBu",   breaks = outcome_breaks, digits = 2)

# --------------------------------------------------------------
# 4. Save
# --------------------------------------------------------------
out <- "."
ggsave(file.path(out, "Figure3_distance.tiff"),   fig3,
       width = 5.2, height = 5.6, dpi = 300, compression = "lzw", bg = "white")
ggsave(file.path(out, "Figure4_depression.tiff"), fig4,
       width = 5.2, height = 5.6, dpi = 300, compression = "lzw", bg = "white")
ggsave(file.path(out, "Figure5_anxiety.tiff"),    fig5,
       width = 5.2, height = 5.6, dpi = 300, compression = "lzw", bg = "white")

cat("\nPlotted values:\n")
print(sf::st_drop_geometry(map_df)[, c("label", "distance", "depression", "anxiety")])
cat("\nFigures 4 and 5 use the same breaks (", paste(outcome_breaks, collapse = ", "),
    ") so they are directly comparable.\n", sep = "")

# --------------------------------------------------------------
# 5. Confirm the files were really rewritten
# --------------------------------------------------------------
for (f in c("Figure3_distance.tiff", "Figure4_depression.tiff", "Figure5_anxiety.tiff")) {
  info <- file.info(f)
  cat(sprintf("%-26s %8.0f bytes  written %s\n", f, info$size, format(info$mtime, "%H:%M:%S")))
}
cat("\nIf the timestamps above are not from the last minute, the plots were not saved.\n")

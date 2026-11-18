# ==============================================================
# Figure 1. Derivation of the analytical sample (STROBE item 13)
#
# Drawn to match Figure 2. The one accent colour is the light blue
# used for the outcome in Figure 2 and for Figures 4 and 5; here it
# marks the final analytical sample. Exclusions stay grey, so the
# colour separates the sample that was analysed from what was not.
#
# All counts are taken from the earlier flow diagram supplied by
# the author. One box is derived rather than copied and is marked
# below: 30,078 - 20,217 = 9,861 women were not covered by the
# mental health module. Check that this wording matches how the
# subsample is described in the Methods before submitting.
# ==============================================================

need <- c("ggplot2")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) stop("Install first: install.packages(c(",
                       paste0('"', miss, '"', collapse = ", "), "))")
library(ggplot2)

# --------------------------------------------------------------
# 1. Boxes down the main column
# --------------------------------------------------------------
main <- data.frame(
  y = 7:1,
  label = c(
    "Households selected for BDHS 2022\nn = 30,330",
    "Households responding\nn = 30,149 (99.4%)",
    "Households interviewed\nn = 30,018 (99.6% of responding households)",
    "Ever-married women identified\nn = 30,358",
    "Women who participated\nn = 30,078 (99.1% of those eligible)",
    "Eligible for the mental health module\nn = 20,217",
    "Final analytical sample\nn = 19,987 (98.9% of those eligible)"
  ),
  stringsAsFactors = FALSE
)

# --------------------------------------------------------------
# 2. Exclusions, shown to the right at the step where they occur
# --------------------------------------------------------------
side <- data.frame(
  y = c(3.5, 2.5, 1.5),
  label = c("Excluded: 280 (0.9%)",
            "Not covered by the mental\nhealth module: 9,861",
            "Excluded: 230 (1.1%)"),
  stringsAsFactors = FALSE
)

BW <- 2.30; BH <- 0.32       # half width and half height of a main box
SW <- 1.35; SH <- 0.26       # half width and half height of a side box
SX <- 3.95                   # centre of the side column

# --------------------------------------------------------------
# 3. Arrows
# --------------------------------------------------------------
down <- data.frame(x = 0, xend = 0,
                   y = main$y[-7] - BH, yend = main$y[-1] + BH + 0.03)
across <- data.frame(x = 0.12, xend = SX - SW - 0.05,
                     y = side$y, yend = side$y)

# --------------------------------------------------------------
# 4. Draw
# --------------------------------------------------------------
p <- ggplot() +
  geom_segment(data = down, aes(x, y, xend = xend, yend = yend),
               colour = "grey20", linewidth = 0.45,
               arrow = arrow(length = unit(2.2, "mm"), type = "closed")) +
  geom_segment(data = across, aes(x, y, xend = xend, yend = yend),
               colour = "grey45", linewidth = 0.35,
               arrow = arrow(length = unit(1.8, "mm"), type = "closed")) +
  # The final box is filled in the same light blue used for the
  # outcome in Figure 2 and for Figures 4 and 5, marking the sample
  # the paper actually analyses. Every other box stays neutral.
  geom_rect(data = main,
            aes(xmin = -BW, xmax = BW, ymin = y - BH, ymax = y + BH,
                fill = y == 1, colour = y == 1, linewidth = y == 1)) +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "#D0D1E6"), guide = "none") +
  scale_colour_manual(values = c("FALSE" = "grey25", "TRUE" = "#034E7B"), guide = "none") +
  scale_linewidth_manual(values = c("FALSE" = 0.45, "TRUE" = 0.8), guide = "none") +
  geom_text(data = main, aes(0, y, label = label),
            size = 2.9, lineheight = 1.05,
            colour = ifelse(main$y == 1, "#034E7B", "black")) +
  geom_rect(data = side,
            aes(xmin = SX - SW, xmax = SX + SW, ymin = y - SH, ymax = y + SH),
            fill = "grey96", colour = "grey45", linewidth = 0.35, linetype = "22") +
  geom_text(data = side, aes(SX, y, label = label),
            size = 2.6, lineheight = 1.05, colour = "grey20") +
  coord_cartesian(xlim = c(-2.6, 5.5), ylim = c(0.45, 7.55), clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(8, 8, 8, 8))

ggsave("Figure1_flowchart.tiff", p, width = 6.6, height = 6.4,
       dpi = 300, compression = "lzw", bg = "white")

# --------------------------------------------------------------
# 5. Arithmetic check and proof the file was written
# --------------------------------------------------------------
cat("\nCheck: 30,078 - 280 participants excluded earlier is already applied.\n")
cat("30,078 - 20,217 =", 30078 - 20217, "(shown as not covered by the module)\n")
cat("20,217 -    230 =", 20217 - 230, "(final analytical sample)\n")

info <- file.info("Figure1_flowchart.tiff")
cat(sprintf("\nFigure1_flowchart.tiff  %8.0f bytes  written %s\n",
            info$size, format(info$mtime, "%H:%M:%S")))

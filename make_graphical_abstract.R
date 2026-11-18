# ==============================================================
# Graphical abstract image (submission field 9)
#
# A forest plot of the six adjusted odds ratios. It is the one
# result the table of contents needs to carry, and a reader who
# knows nothing about the survey can read it in a few seconds:
# everything sits to the right of 1, and the three samples agree.
#
# Colours follow the rest of the figure set. Both outcomes stay in
# the cool family used for Figures 4 and 5, separated by lightness
# rather than by hue, so the plot survives greyscale printing.
# ==============================================================

need <- c("ggplot2")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) stop("Install first: install.packages(c(",
                       paste0('"', miss, '"', collapse = ", "), "))")
library(ggplot2)

# --------------------------------------------------------------
# Adjusted odds ratios, from Table 2
# --------------------------------------------------------------
d <- data.frame(
  sample  = factor(rep(c("National\n(n = 19,987)", "Rural\n(n = 12,980)", "Urban\n(n = 7,007)"), each = 2),
                   levels = rev(c("National\n(n = 19,987)", "Rural\n(n = 12,980)", "Urban\n(n = 7,007)"))),
  outcome = factor(rep(c("Depression", "Anxiety"), 3), levels = c("Depression", "Anxiety")),
  or      = c(1.39, 1.40, 1.39, 1.40, 1.45, 1.42),
  lo      = c(1.16, 1.14, 1.11, 1.09, 1.06, 0.99),
  hi      = c(1.68, 1.73, 1.73, 1.81, 1.99, 2.06)
)

p <- ggplot(d, aes(x = or, y = sample, colour = outcome, shape = outcome)) +
  geom_vline(xintercept = 1, colour = "grey45", linetype = "22", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.16, linewidth = 0.6,
                 position = position_dodge(width = 0.55)) +
  geom_point(size = 2.6, position = position_dodge(width = 0.55)) +
  scale_x_continuous(trans = "log", breaks = c(0.9, 1, 1.25, 1.5, 2),
                     labels = c("0.9", "1", "1.25", "1.5", "2"),
                     limits = c(0.88, 2.2)) +
  scale_colour_manual(values = c(Depression = "#034E7B", Anxiety = "#74A9CF"), name = NULL) +
  scale_shape_manual(values = c(Depression = 16, Anxiety = 17), name = NULL) +
  labs(
    title    = "Distance to a health facility and women's mental health in Bangladesh",
    subtitle = "Adjusted odds ratios for women reporting distance as a big problem,\nagainst those who did not",
    x = "Adjusted odds ratio (log scale)", y = NULL,
    caption  = "BDHS 2022, 19,987 ever-married women aged 15-49 years"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
    axis.text.y        = element_text(size = 8.5, lineheight = 0.95, hjust = 1),
    plot.title         = element_text(size = 11, face = "bold"),
    plot.subtitle      = element_text(size = 8.8, colour = "grey30", lineheight = 1.1),
    plot.caption       = element_text(size = 7.5, colour = "grey45"),
    legend.position    = "top",
    legend.justification = "left",
    legend.margin      = margin(0, 0, -4, 0),
    plot.background    = element_rect(fill = "white", colour = NA),
    plot.margin        = margin(8, 10, 6, 6)
  )

ggsave("GraphicalAbstract.tiff", p, width = 6.6, height = 3.4,
       dpi = 300, compression = "lzw", bg = "white")

info <- file.info("GraphicalAbstract.tiff")
cat(sprintf("\nGraphicalAbstract.tiff  %8.0f bytes  written %s\n",
            info$size, format(info$mtime, "%H:%M:%S")))
cat("All six intervals should sit to the right of 1 except urban anxiety, which crosses it.\n")

# ==============================================================
# Figure 2. Directed acyclic graph
#
# Colours are drawn from the map palettes so that Figures 1 to 5
# look like one set. Each colour marks a role; nothing is coloured
# for decoration.
#
# Node names passed to dagitty are plain one-line strings. The
# dagitty parser rejects a newline inside a quoted name, so the
# line breaks shown in the figure come from a separate lookup and
# are applied only when drawing.
#
# Edges are drawn with geom_segment rather than geom_dag_edges, so
# each arrow stops at the edge of its box and its head stays
# visible. The exposure to outcome arrow is drawn darker and
# thicker because it is the relationship being estimated; the
# covariate arrows are the assumptions behind the adjustment.
#
# Covariates are grouped into four domains. All sixteen enter the
# model with the same assumed structure, each affecting both the
# access barrier and the outcomes, so sixteen separate nodes would
# give thirty-three crossing arrows and no extra information. The
# membership of each domain is printed as the caption at the end.
#
# Dropped from the earlier hand-drawn version: father education
# (it reproduced the respondent's own education and made the two
# education terms collinear) and household assets (those are the
# components from which the DHS wealth index is built).
# ==============================================================

need <- c("dagitty", "ggplot2")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) stop("Install first: install.packages(c(",
                       paste0('"', miss, '"', collapse = ", "), "))")
library(dagitty); library(ggplot2)

# --------------------------------------------------------------
# 1. Nodes
# --------------------------------------------------------------
A <- "Access barrier"
Y <- "Depression and anxiety"
dom <- c("Woman characteristics", "Reproductive factors",
         "Household circumstances", "Place and norms")

members <- list(
  "Woman characteristics"   = c("age", "education", "current employment"),
  "Reproductive factors"    = c("number of living children", "current pregnancy",
                                "menstruation in the last six weeks",
                                "history of pregnancy loss"),
  "Household circumstances" = c("wealth index", "house ownership", "household size",
                                "sex of the household head", "housing materials",
                                "internet use"),
  "Place and norms"         = c("place of residence", "division",
                                "justification of intimate partner violence")
)

display <- c(
  "Access barrier"          = "Difficulty in\naccessing health care",
  "Depression and anxiety"  = "Depression\nand anxiety",
  "Woman characteristics"   = "Woman's\ncharacteristics",
  "Reproductive factors"    = "Reproductive\nfactors",
  "Household circumstances" = "Household\ncircumstances",
  "Place and norms"         = "Place and\nnorms"
)

nodes <- data.frame(
  name = c(dom, A, Y),
  x    = c(-2.30, -0.80, 0.80, 2.30, -2.95, 2.95),
  y    = c( 1.30,  1.95, 1.95, 1.30,  0,    0),
  role = c(rep("domain", 4), "exposure", "outcome"),
  stringsAsFactors = FALSE
)
nodes$lab <- display[nodes$name]

# --------------------------------------------------------------
# 2. DAG object (for the adjustment set check)
# --------------------------------------------------------------
edges_txt <- c(paste0('"', dom, '" -> "', A, '"'),
               paste0('"', dom, '" -> "', Y, '"'),
               paste0('"', A, '" -> "', Y, '"'))
dag <- dagitty(paste0("dag { ", paste(edges_txt, collapse = " "), " }"))
exposures(dag) <- A
outcomes(dag)  <- Y
coordinates(dag) <- list(x = setNames(nodes$x, nodes$name),
                         y = setNames(nodes$y, nodes$name))

# --------------------------------------------------------------
# 3. Edge geometry: stop each arrow at the edge of its box
# --------------------------------------------------------------
halfw <- 0.50   # half box width in data units
halfh <- 0.155  # half box height
pad   <- 1.10   # small gap so the head does not touch the box

trim <- function(x1, y1, x2, y2) {
  dx <- x2 - x1; dy <- y2 - y1
  s1 <- min(halfw / max(abs(dx), 1e-9), halfh / max(abs(dy), 1e-9)) * pad
  s2 <- s1
  data.frame(x = x1 + dx * s1, y = y1 + dy * s1,
             xend = x2 - dx * s2, yend = y2 - dy * s2)
}

mk <- function(from, to, kind) {
  f <- nodes[nodes$name == from, ]; t <- nodes[nodes$name == to, ]
  cbind(trim(f$x, f$y, t$x, t$y), kind = kind)
}
ed <- do.call(rbind, c(
  lapply(dom, mk, to = A, kind = "assumption"),
  lapply(dom, mk, to = Y, kind = "assumption"),
  list(mk(A, Y, "estimated"))
))

# --------------------------------------------------------------
# 4. Draw
# --------------------------------------------------------------
p <- ggplot() +
  geom_segment(data = subset(ed, kind == "assumption"),
               aes(x, y, xend = xend, yend = yend),
               colour = "grey60", linewidth = 0.38,
               arrow = arrow(length = unit(1.9, "mm"), type = "closed")) +
  geom_segment(data = subset(ed, kind == "estimated"),
               aes(x, y, xend = xend, yend = yend),
               colour = "grey15", linewidth = 0.75,
               arrow = arrow(length = unit(2.6, "mm"), type = "closed")) +
  geom_label(data = nodes, aes(x, y, label = lab, fill = role,
                               colour = role, linewidth = role),
             size = 3.0, lineheight = 0.95,
             label.r = unit(1.3, "mm"), label.padding = unit(1.9, "mm")) +
  # Colours are taken from the two palettes used for the maps, so the
  # five figures read as one set, and each colour marks a role rather
  # than decorating: the warm pair is the exposure, carried over from
  # Figure 3, and the cool pair is the outcome, carried over from
  # Figures 4 and 5. Covariate domains stay neutral.
  scale_fill_manual(values = c(exposure = "#FEE391",   # YlOrBr light
                               outcome  = "#D0D1E6",   # PuBu light
                               domain   = "white"), guide = "none") +
  scale_colour_manual(values = c(exposure = "#8C2D04", # YlOrBr dark
                                 outcome  = "#034E7B", # PuBu dark
                                 domain   = "grey25"), guide = "none") +
  scale_linewidth_manual(values = c(exposure = 0.8, outcome = 0.8,
                                    domain = 0.35), guide = "none") +
  coord_cartesian(xlim = c(-3.7, 3.7), ylim = c(-0.5, 2.45), clip = "off") +
  theme_void(base_size = 10) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(8, 8, 8, 8))

ggsave("Figure2_DAG.tiff", p, width = 6.8, height = 4.0,
       dpi = 300, compression = "lzw", bg = "white")

# --------------------------------------------------------------
# 5. Caption, adjustment set, and proof the file was written
# --------------------------------------------------------------
cat("\n--- Caption for Figure 2 ---\n")
cat("Figure 2. Assumed relationships between difficulty in accessing health care, ",
    "depression and anxiety, and the covariates entered in the adjusted models. ",
    "The dark arrow is the association estimated in this study. ",
    paste0(vapply(seq_along(members), function(i)
      paste0(names(members)[i], ": ", paste(members[[i]], collapse = ", ")),
      character(1)), collapse = ". "), ".\n", sep = "")

cat("\n--- Minimal sufficient adjustment set for the total effect ---\n")
print(adjustmentSets(dag, effect = "total"))

info <- file.info("Figure2_DAG.tiff")
cat(sprintf("\nFigure2_DAG.tiff  %8.0f bytes  written %s\n",
            info$size, format(info$mtime, "%H:%M:%S")))
cat("Covariates represented:", sum(lengths(members)), "\n")

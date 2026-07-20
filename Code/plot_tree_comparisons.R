library(igraph)
library(ggraph)
library(ggplot2)
library(patchwork)

source("Functions/distinction.R")

# Ensure Plots directory exists
if (!dir.exists("Plots")) {
  dir.create("Plots")
}

# Helper function to plot
plot_tree <- function(g, title, layout = "tree") {
  V(g)$name <- as.character(1:vcount(g))
  res <- distinction(g, norm = "abm")
  
  ggraph(g, layout = layout) +
    geom_edge_link(color = "gray50", width = 1) +
    geom_node_point(aes(fill = res$scd), shape = 21, size = 10, color = "black") +
    geom_node_text(aes(label = name), color = "white", fontface = "bold", size = 4) +
    scale_fill_viridis_c(name = "Scaled\nDist.", option = "plasma") +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      legend.position = "bottom"
    )
}

# 1. Depth 1 (Star Graph)
g_star <- make_star(10, mode = "undirected")
p_star <- plot_tree(g_star, "Depth 1 (Star Graph)", layout = "star")

# 2. Depth 2 (Binary Tree N=7)
g_d2 <- make_tree(n = 7, children = 2, mode = "undirected")
p_d2 <- plot_tree(g_d2, "Depth 2 (Binary, N=7)")

# 3. Depth 3 (Binary Tree N=15)
g_d3 <- make_tree(n = 15, children = 2, mode = "undirected")
p_d3 <- plot_tree(g_d3, "Depth 3 (Binary, N=15)")

# Combine plots using patchwork
p_combined <- p_star + p_d2 + p_d3 + plot_layout(ncol = 3)

ggsave("Plots/tree_comparisons.png", p_combined, width = 12, height = 5, dpi = 300)
print("Saved tree comparisons plot to Plots/tree_comparisons.png")

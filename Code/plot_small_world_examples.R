library(igraph)
library(ggraph)
library(ggplot2)
library(patchwork)

source("Functions/distinction.R")

set.seed(42)
N <- 30

# We want 3 snapshots:
# 1. Initial Lattice
# 2. Small World (Density ~ 0.15 - 0.20)
# 3. Dense Random (Density ~ 0.50)

# Helper function to plot
plot_network_snap <- function(g, title) {
  V(g)$name <- as.character(1:vcount(g))
  res <- distinction(g, norm = "abm")
  
  # For the layout, we will use a circular layout for all to show the chords clearly
  layout_coords <- layout_in_circle(g)
  
  ggraph(g, layout = layout_coords) +
    geom_edge_link(color = "gray80", width = 0.5, alpha = 0.7) +
    geom_node_point(aes(fill = res$scd), shape = 21, size = 6, color = "black") +
    scale_fill_gradient2(low = "dodgerblue", mid = "gray90", high = "firebrick", name = "Scaled\nDist.") +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      legend.position = "bottom"
    )
}

# 1. Initial Lattice
g1 <- make_ring(N)
p1 <- plot_network_snap(g1, "1. Initial Lattice (Density: 0.07)")

# 2. Small World
# Let's add 25 random edges
g2 <- g1
full_g <- make_full_graph(N)
missing_edges <- difference(full_g, g2)
missing_edge_list <- as_edgelist(missing_edges)
missing_edge_list <- missing_edge_list[sample(nrow(missing_edge_list)), ]

g2 <- add_edges(g2, as.vector(t(missing_edge_list[1:25, ])))
p2 <- plot_network_snap(g2, "2. Small World (Density: ~0.13)")

# 3. Dense Random
# Add 200 random edges total
g3 <- add_edges(g1, as.vector(t(missing_edge_list[1:200, ])))
p3 <- plot_network_snap(g3, "3. Dense Random (Density: ~0.53)")

# Combine
p_combined <- p1 + p2 + p3 + plot_layout(ncol = 3, guides = "collect") & theme(legend.position = 'bottom')

ggsave("Plots/small_world_snapshots.png", p_combined, width = 12, height = 4.5, dpi = 300)
print("Saved small world snapshots to Plots/small_world_snapshots.png")

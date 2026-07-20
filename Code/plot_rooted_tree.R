library(igraph)
library(ggraph)
library(ggplot2)

source("Functions/distinction.R")

# Ensure Plots directory exists
if (!dir.exists("Plots")) {
  dir.create("Plots")
}

# Create a rooted binary tree with 7 nodes (root + 2 children + 4 leaves)
t2_7 <- make_tree(n = 7, children = 2, mode = "undirected")
V(t2_7)$name <- as.character(1:vcount(t2_7))

# Compute distinction metrics
res_tree <- distinction(t2_7, norm = "abm")

# Generate a visual plot mapping distinction to node color
p <- ggraph(t2_7, layout = "tree", root = 1) +
  geom_edge_link(color = "gray50", width = 1) +
  geom_node_point(aes(fill = res_tree$scd), shape = 21, size = 12, color = "black") +
  geom_node_text(aes(label = name), color = "black", fontface = "bold") +
  scale_fill_gradient2(low = "dodgerblue", mid = "gray90", high = "firebrick", name = "Scaled\nDistinction") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right"
  )

ggsave("Plots/rooted_tree.png", p, width = 6, height = 5, dpi = 300)
print("Saved tree plot to Plots/rooted_tree.png")

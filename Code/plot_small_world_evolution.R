library(igraph)
library(dplyr)
library(ggplot2)

source("Functions/distinction.R")

# Ensure Plots directory exists
if (!dir.exists("Plots")) {
  dir.create("Plots")
}

set.seed(42)
N <- 30
# Start with a regular lattice (ring)
g <- make_ring(N)

# Total possible edges in a full graph
max_edges <- N * (N - 1) / 2
current_edges <- ecount(g)
edges_to_add <- max_edges - current_edges

# We will add edges and track metrics
steps <- 50
edges_per_step <- floor(edges_to_add / steps)

# Generate a list of all possible non-existent edges
full_g <- make_full_graph(N)
missing_edges <- difference(full_g, g)
missing_edge_list <- as_edgelist(missing_edges)

# Randomize the order of missing edges to add
missing_edge_list <- missing_edge_list[sample(nrow(missing_edge_list)), ]

results <- list()

# Initial state
res <- distinction(g, norm = "abm")
results[[1]] <- data.frame(
  step = 0,
  edges = ecount(g),
  density = edge_density(g),
  mean_u = mean(res$u),
  sd_u = sd(res$u),
  sd_scd = sd(res$scd)
)

edge_idx <- 1
for (i in 1:steps) {
  # Edges to add in this step
  n_add <- min(edges_per_step, nrow(missing_edge_list) - edge_idx + 1)
  if (n_add <= 0) break
  
  edges_to_add_now <- missing_edge_list[edge_idx:(edge_idx + n_add - 1), ]
  edge_idx <- edge_idx + n_add
  
  # Add edges to g
  g <- add_edges(g, as.vector(t(edges_to_add_now)))
  
  res <- distinction(g, norm = "abm")
  results[[i + 1]] <- data.frame(
    step = i,
    edges = ecount(g),
    density = edge_density(g),
    mean_u = mean(res$u),
    sd_u = sd(res$u),
    sd_scd = sd(res$scd)
  )
}

# Final state (make sure it's fully connected)
if (ecount(g) < max_edges) {
  g <- make_full_graph(N)
  res <- distinction(g, norm = "abm")
  results[[length(results) + 1]] <- data.frame(
    step = steps + 1,
    edges = ecount(g),
    density = edge_density(g),
    mean_u = mean(res$u),
    sd_u = sd(res$u),
    sd_scd = sd(res$scd)
  )
}

df_sim <- bind_rows(results)

# Plot Variance of Distinction
p_sd_scd <- ggplot(df_sim, aes(x = density, y = sd_scd)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 1.5) +
  labs(x = "Network Density", y = "Standard Deviation of Scaled Distinction") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave("Plots/variance_vs_density.png", p_sd_scd, width = 6, height = 4, dpi = 300)

# Plot Mean Constraint
p_mean_u <- ggplot(df_sim, aes(x = density, y = mean_u)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 1.5) +
  labs(x = "Network Density", y = "Mean Constraint") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave("Plots/mean_constraint_vs_density.png", p_mean_u, width = 6, height = 4, dpi = 300)

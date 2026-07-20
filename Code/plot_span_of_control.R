library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)

source("Functions/distinction.R")

# Ensure Plots directory exists
if (!dir.exists("Plots")) {
  dir.create("Plots")
}

spans <- 2:10
results <- list()

for (k in spans) {
  # Calculate total nodes for a depth 2 tree with branching factor k:
  # 1 root + k children + k^2 leaves = 1 + k + k^2
  n_nodes <- 1 + k + k^2
  g <- make_tree(n = n_nodes, children = k, mode = "undirected")
  V(g)$name <- as.character(1:vcount(g))
  
  res <- distinction(g, norm = "abm")
  
  # Root is always node 1
  root_scd <- res$scd[1]
  
  # A middle branch is node 2
  mid_scd <- res$scd[2]
  
  alpha <- res$scalar[1]
  
  results[[length(results) + 1]] <- data.frame(
    Span = k,
    Root = root_scd,
    `Middle Manager` = mid_scd,
    Alpha = alpha,
    check.names = FALSE
  )
}

df <- bind_rows(results)

df_long <- pivot_longer(df, cols = c("Root", "Middle Manager"), names_to = "Role", values_to = "Distinction")

p <- ggplot(df_long, aes(x = Span, y = Distinction, color = Role, group = Role)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Root" = "dodgerblue", "Middle Manager" = "firebrick")) +
  scale_x_continuous(breaks = spans) +
  labs(
    x = "Span of Control (Branching Factor)",
    y = "Scaled Distinction"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 11)
  )

ggsave("Plots/span_of_control_distinction.png", p, width = 6, height = 4, dpi = 300)
print("Saved span of control plot to Plots/span_of_control_distinction.png")

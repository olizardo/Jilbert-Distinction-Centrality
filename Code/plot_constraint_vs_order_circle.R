library(igraph)
library(ggplot2)

# Load the distinction centrality function
source("Functions/distinction.R")

# We will calculate the average constraint for circle graphs of sizes 4 through 50
orders <- 4:50
constraints <- numeric(length(orders))

# Compute distinction using the "abm" normalization
for (i in seq_along(orders)) {
  g <- make_ring(orders[i])
  res <- distinction(g, norm = "abm")
  # Since all nodes in a circle graph are symmetric, mean constraint is the same as individual constraint
  constraints[i] <- mean(res$u)
}

# Create a dataframe for plotting
df <- data.frame(Order = orders, Constraint = constraints)

# Generate the plot
p <- ggplot(df, aes(x = Order, y = Constraint)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "black", size = 2) +
  labs(
    x = "Order of Circle Graph (N)",
    y = "Average Constraint"
  ) +
  scale_x_continuous(breaks = c(4, seq(10, 50, by = 10))) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Ensure the Plots directory exists (it should, based on project structure)
if (!dir.exists("Plots")) {
  dir.create("Plots")
}

# Save the plot
ggsave("Plots/constraint_vs_order_circle.png", p, width = 6, height = 4, dpi = 300)

print("Plot saved successfully to Plots/constraint_vs_order_circle.png")

library(igraph)
source("Functions/distinction.R")

for (k in 1:6) {
  n_nodes <- 1 + k + k^2 + k^3 + k^4
  cat("k =", k, "n_nodes =", n_nodes, "\n")
  # g <- make_tree(n = n_nodes, children = k, mode = "undirected")
  # res <- distinction(g)
}

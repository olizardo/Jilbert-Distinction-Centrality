library(igraph)

source("Functions/distinction.R")

# Create a sample graph
g <- make_star(10, mode="undirected")
# Test current distinction
t1 <- Sys.time()
res1 <- distinction(g)
t2 <- Sys.time()
print(t2 - t1)
print(res1)


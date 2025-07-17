diss.status <- function(x) { 
   library(igraph)
   has.labels <- as.numeric(is.null(V(x)$name) == FALSE)
   V <- vcount(x)
   if (has.labels == 0) {
      V(x)$name <- 1:V
      names <- V(x)$name 
      }
   if (has.labels == 1) {
      names <- V(x)$name 
      V(x)$name <- 1:V
      }
  vertex.sim <- function(w) {
      z <- as.matrix(as_adjacency_matrix(w))
      z.p <- z %*% z
      z.q <- z %*% (1 - z) 
      z.r <- (1 - z) %*% z
      s <- z.p / (z.p + z.q + z.r)
      return(list(sim = s, adj = z))
      }
  sim.res <- vertex.sim(x)
  d.s <- abs(eigen(sim.res$adj * (1 - sim.res$sim))$vectors[, 1])
  names(d.s) <- V(x)$name
  return(d.s/max(d.s))
}
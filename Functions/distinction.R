distinction <- function(x, norm = "abm", digits = 4) {
  require(igraph)
  n <- vcount(x)
  
  if (is.null(V(x)$name)) {
    V(x)$name <- 1:n
    names <- V(x)$name
  } else {
    names <- V(x)$name
    V(x)$name <- 1:n
  }
  
  norm.max <- function(v) { v^2 / max(v^2) }
  norm.one <- function(v) { v^2 }
  norm.abm <- function(v) { abs(v) / max(abs(v)) }
  norm.abs <- function(v) { abs(v) }
  
  # Helper to compute status for any graph (handles components and isolates robustly)
  get_status <- function(g) {
    nv <- vcount(g)
    s_vec <- numeric(nv)
    xc <- components(g)
    for (k in 1:xc$no) {
      nodes_in_comp <- which(xc$membership == k)
      if (length(nodes_in_comp) > 1) {
        subg <- induced_subgraph(g, nodes_in_comp)
        eig_sub <- eigen(as_adjacency_matrix(subg, sparse = FALSE))
        s_vec[nodes_in_comp] <- abs(eig_sub$vectors[, 1])
      } else {
        s_vec[nodes_in_comp] <- 0
      }
    }
    return(s_vec)
  }
  
  # Compute main graph status
  s <- get_status(x)
  
  # Normalize status
  if (norm == "max") { s <- norm.max(s) }
  if (norm == "one") { s <- norm.one(s) }
  if (norm == "abm") { s <- norm.abm(s) }
  if (norm == "abs") { s <- norm.abs(s) }
  
  d <- numeric(n)
  u <- numeric(n)
  
  for (i in 1:n) {
    j <- neighbors(x, i)
    xd <- delete_vertices(x, i)
    ed <- ecount(xd) == 0
    nd <- vcount(xd)
    
    if (ed == 0 & nd > 1) {
      sd <- get_status(xd)
      if (norm == "max") { sd <- norm.max(sd) }
      if (norm == "one") { sd <- norm.one(sd) }
      if (norm == "abm") { sd <- norm.abm(sd) }
      if (norm == "abs") { sd <- norm.abs(sd) }
    } else {
      sd <- rep(0, nd)
    }
    
    names(sd) <- V(xd)$name
    u[i] <- mean(sd[as.character(j)])
    d[i] <- s[i] - u[i]
  }
  
  d[is.na(d)] <- 0
  u[is.na(u)] <- 0
  scalar <- mean(u) / mean(s)
  scd <- (s * scalar) - u
  
  dat <- data.frame(d = d, scd = scd, s = s, u = u, scalar = scalar)
  dat <- round(dat, digits)
  dat <- data.frame(n = names, dat)
  rownames(dat) <- 1:nrow(dat)
  return(dat)
}

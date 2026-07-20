distinction <- function(x, norm = "abm", digits = 4) {
  n <- vcount(x)
  
  # Ensure vertices have sequential numeric names for indexing, preserving original labels
  if (is.null(V(x)$name)) {
    V(x)$name <- as.character(1:n)
    names <- V(x)$name
  } else {
    names <- V(x)$name
    V(x)$name <- as.character(1:n)
  }
  
  # Normalization functions for status (eigenvector centrality) scores
  # Handled division by zero for sparse/empty subgraphs
  apply_norm <- function(v, norm_type) {
    if (norm_type == "max") {
      if (max(v^2) == 0) v else v^2 / max(v^2)
    } else if (norm_type == "one") {
      v^2
    } else if (norm_type == "abm") {
      if (max(abs(v)) == 0) v else abs(v) / max(abs(v))
    } else if (norm_type == "abs") {
      abs(v)
    } else {
      v
    }
  }
  
  # Helper to compute status for any graph (handles components and isolates robustly)
  get_status <- function(g) {
    nv <- vcount(g)
    s_vec <- numeric(nv)
    xc <- components(g)
    # Process each component independently to avoid global eigen problems
    for (k in 1:xc$no) {
      nodes_in_comp <- which(xc$membership == k)
      if (length(nodes_in_comp) > 1) {
        subg <- induced_subgraph(g, nodes_in_comp)
        # Replaced base::eigen with igraph::eigen_centrality (ARPACK) for O(E) efficiency instead of O(V^3)
        eig <- suppressWarnings(eigen_centrality(subg, scale = FALSE)$vector)
        s_vec[nodes_in_comp] <- abs(eig)
      } else {
        s_vec[nodes_in_comp] <- 0
      }
    }
    return(s_vec)
  }
  
  # 1. Compute main graph status (s)
  s <- get_status(x)
  
  # 2. Normalize main status
  s <- apply_norm(s, norm)
  
  d <- numeric(n) # Distinction vector
  u <- numeric(n) # Constraint vector (average neighbor status in deleted subgraph)
  
  # 3. Iterate through each node to calculate induced constraint (jackknife-like approach)
  for (i in 1:n) {
    j <- neighbors(x, i)
    
    # Optimization: Skip subgraph eigen decomposition if the focal node has no neighbors
    if (length(j) == 0) {
      u[i] <- 0
      d[i] <- s[i]
      next
    }
    
    xd <- delete_vertices(x, i)
    
    # Compute status of neighbors in the node-deleted subgraph (xd)
    # Fixed boolean coercion bug (was checking if FALSE == 0)
    if (ecount(xd) > 0 && vcount(xd) > 1) {
      sd <- get_status(xd)
      # Re-apply same normalization to deleted subgraph status
      sd <- apply_norm(sd, norm)
    } else {
      sd <- rep(0, vcount(xd))
    }
    
    names(sd) <- V(xd)$name
    # Constraint (u) = mean status of focal node's neighbors in deleted graph
    # Robust indexing using as_ids() to fetch original names
    u[i] <- mean(sd[as.character(as_ids(j))])
    d[i] <- s[i] - u[i] # Distinction (d) = Status - Constraint
  }
  
  # 4. Final aggregation and scalar-adjusted distinction (scd) calculation
  d[is.na(d)] <- 0
  u[is.na(u)] <- 0
  
  # Handled division by zero if entire network has 0 status
  mean_s <- mean(s)
  scalar <- if (mean_s == 0) 0 else mean(u) / mean_s
  scd <- (s * scalar) - u
  
  # Assemble output dataframe
  dat <- data.frame(d = d, scd = scd, s = s, u = u, scalar = scalar)
  dat <- round(dat, digits)
  dat <- data.frame(n = names, dat)
  rownames(dat) <- 1:nrow(dat)
  return(dat)
}

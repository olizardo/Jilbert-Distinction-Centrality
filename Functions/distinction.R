distinction <- function(x, norm = "max", digits  = 4) { 
  require(igraph) #need igraph
  n <- vcount(x)
  if (is.null(V(x)$name) == 1) {
    V(x)$name <- 1:n
    names <- V(x)$name 
    }
  if (is.null(V(x)$name) == 0) {
    names <- V(x)$name 
    V(x)$name <- 1:n
    }
  norm.max <- function(x) {return(x^2/max(x^2))} #sum to one and max normalizing function
  norm.one <- function(x) {return(x^2)} #sum to one normalizing function
  norm.abm <- function(x) {return(abs(x)/max(abs(x)))} #max normalizing function
  norm.abs <- function(x) {return(abs(x))} #absolute value normalizing function
  eig.x <- eigen(as_adjacency_matrix(x)) #eigenvector decomposition of adjacency matrix
  nc <- components(x)$no #number of components
  print(nc)
  if (nc == 1) {s <- eig.x$vectors[, 1]} #collecting status scores from first eigenvector in connected graph
  if (nc > 1) {s <- rowSums(eig.x$vectors[, 1:nc])} #collecting status scores from separate eigenvectors in disconnected graph
  if (norm == "max") {s <- norm.max(s)} #normalizing so that maximum value is one
  if (norm == "one") {s <- norm.one(s)} #normalizing so that vector sums to one
  if (norm == "abm") {s <- norm.abm(s)} #normalizing so that maximum value is one
  if (norm == "abs") {s <- norm.abs(s)} #normalizing to remove negative entries
  d <- rep(n, 0) # initializing distinction vector to all zeros
  u <- rep(n, 0) # initializing constraint vector to all zeros
  for (i in 1:n) { #start looping across nodes in the graph
    j <- as.character(neighbors(x, i))
    xd <- delete_vertices(x, i) #node-deleted subgraph (minus i)
    ncd <- components(xd)$no #number of components of node-deleted subgraph
    eig.xd <- eigen(as_adjacency_matrix(xd)) # eigenvector decomposition of node-deleted subgraph
    if (ncd == 1) {sd <- eig.xd$vectors[, 1]}
    if (ncd > 1) {sd <- rowSums(eig.xd$vectors[, 1:ncd])}
    ed <- ecount(xd) == 0 #checking for emptiness of node-deleted subgraph
    nd <- vcount(xd) #number of nodes of node delete subgraph
    if (ed == 0 &  nd > 1 & norm == "max") {sd <- norm.max(sd)} #non-empty node-deleted subgraph
    if (ed == 0 & nd > 1 & norm == "one") {sd <- norm.one(sd)} #non-empty node-deleted subgraph
    if (ed == 0 & nd > 1 & norm == "abm") {sd <- norm.abm(sd)} #non-empty node-deleted subgraph
    if (ed == 0 & nd > 1 & norm == "abs") {sd <- norm.abs(sd)} #non-empty node-deleted subgraph
    if (nd == 1) {sd <- 0} #singleton node-deleted subgraph
    if (ed == 1 & nd > 1) {sd <- rep(0, nd)} #empty node-deleted subgraph
    names(sd) <- V(xd)$name
    u[i] <- mean(sd[as.character(j)], na.rm = TRUE) #i's average neighbor eigenvector centrality in node deleted subgraph
    d[i] <- s[i] - u[i] #i's distinction centrality
    } #end for loop
  d[is.na(d)] <- 0 #replacing NAs with zero
  u[is.na(u)] <- 0 #replacing NAs with zero
  scalar <- mean(u)/mean(s) #scalar
  sd <- (s*scalar) - u #scaled distinction
  dat <- data.frame(d = d, sd = sd, s = s, u = u, scalar = scalar) #data frame
  dat <- round(dat, digits) #rounding values
  dat <- data.frame(n = names, dat) #adding node names
  rownames(dat) <- 1:nrow(dat) #adding row names
  return(dat)
} #end function
distinction <- function(x, norm = "abm", digits  = 4) { 
  require(igraph) #need igraph
  n <- vcount(x) #number of nodes in the graph
  if (is.null(V(x)$name) == 1) { #checking for node labels
    V(x)$name <- 1:n #labeling nodes with consecutive numbers
    names <- V(x)$name #storing new labels in vector
    }
  if (is.null(V(x)$name) == 0) { #if the nodes already have labels
    names <- V(x)$name #store old labels in vector
    V(x)$name <- 1:n #label nodes with consecutive numbers
    } 
  norm.max <- function(x) {x^2/max(x^2)} #sum to one and max one normalizing function
  norm.one <- function(x) {x^2} #sum to one normalizing function
  norm.abm <- function(x) {abs(x)/max(abs(x))} #absolute value max one normalizing function
  norm.abs <- function(x) {abs(x)} #absolute value normalizing function
  eig.x <- eigen(as_adjacency_matrix(x)) #eigenvector decomposition of adjacency matrix
  xc <- components(x) #graph component analysis
  nc <- xc$no #number of components in graph
  nn <- sum(xc$csize != 1) #number of non-isolated components in graph
  ni <- nc - nn #number of isolated components in the graph
  ci <- which(xc$csize != 1) #column indices for non-isolated components
  if (nc == 1) {s <- eig.x$vectors[, 1]} #collecting status scores from first eigenvector in connected graph
  if (nc > 1 & nn == 1) {s <- eig.x$vectors[, 1]} #collecting status scores from first eigenvector in disconnected graph with a single connected component
  if (nc > 1 & ni == 1) {s <- eig.x$vectors[, 1]} #collecting status scores from first eigenvector in disconnected graph with a single isolate
  if (nc > 1 & nn > 1) {s <- rowSums(eig.x$vectors[, ci])} #collecting status scores from the sum of the first k eigenvectors in disconnected graph with multiple components and no isolates
  if (norm == "max") {s <- norm.max(s)} #normalizing so that maximum value is one
  if (norm == "one") {s <- norm.one(s)} #normalizing so that vector sums to one
  if (norm == "abm") {s <- norm.abm(s)} #normalizing so that maximum value is one
  if (norm == "abs") {s <- norm.abs(s)} #normalizing to remove negative entries
  d <- rep(n, 0) # initializing distinction vector to all zeros
  u <- rep(n, 0) # initializing constraint vector to all zeros
  for (i in 1:n) { #start looping across nodes in the graph
    j <- neighbors(x, i) #vector of i's neighbors ids
    xd <- delete_vertices(x, i) #node-deleted subgraph (minus i)
    ed <- ecount(xd) == 0 #checking for empty node-deleted subgraph
    nd <- vcount(xd) #number of nodes of node delete subgraph
    ncd <- components(xd)$no #number of components of node-deleted subgraph
    eig.xd <- eigen(as_adjacency_matrix(xd)) #eigenvector decomposition of node-deleted subgraph
    xdc <- components(xd) #subgraph component analysis
    nnd <- sum(xdc$csize != 1) #number of non-isolated components in graph
    nid <- ncd - nnd
    cid <- which(components(xd)$csize != 1) #column indices for non-isolated components
    if (ncd == 1) {sd <- eig.xd$vectors[, 1]} #collecting status scores from first eigenvector in connected subgraph
    if (ncd > 1 & nnd == 1) {sd <- eig.xd$vectors[, 1]} #collecting status scores from first eigenvector in disconnected subgraph with a single connected component
    if (ncd > 1 & nid == 1) {sd <- eig.xd$vectors[, 1]} #collecting status scores from first eigenvector in disconnected subgraph with a single isolate
    if (ncd > 1 & nnd > 1) {sd <- rowSums(eig.xd$vectors[, cid])} #collecting status scores from the sum of the first k eigenvectors in disconnected subgraph with multiple components and no isolates
    if (ed == 0 & nd > 1 & norm == "max") {sd <- norm.max(sd)} #normalizing so that maximum value is one
    if (ed == 0 & nd > 1 & norm == "one") {sd <- norm.one(sd)} #normalizing so that vector sums to one
    if (ed == 0 & nd > 1 & norm == "abm") {sd <- norm.abm(sd)} #normalizing so that maximum value is one
    if (ed == 0 & nd > 1 & norm == "abs") {sd <- norm.abs(sd)} #normalizing to remove negative entries
    if (ed == 1 & nd > 1 | nd == 1) {sd <- rep(0, nd)} #empty or singleton node-deleted subgraph get status score of zero
    names(sd) <- V(xd)$name #naming status score vector for node-deleted subgraph
    u[i] <- mean(sd[as.character(j)]) #i's average neighbor eigenvector centrality in node deleted subgraph
    d[i] <- s[i] - u[i] #i's distinction centrality
    } #end for loop
  d[is.na(d)] <- 0
  u[is.na(u)] <- 0
  scalar <- mean(u)/mean(s) #scalar
  scd <- (s*scalar) - u #scaled distinction
  dat <- data.frame(d = d, scd = scd, s = s, u = u, scalar = scalar) #data frame
  dat <- round(dat, digits) #rounding values
  dat <- data.frame(n = names, dat) #adding node names
  rownames(dat) <- 1:nrow(dat) #adding row names
  return(dat)
} #end function


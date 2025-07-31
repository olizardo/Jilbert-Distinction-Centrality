distinction <- function(x, norm = "max", digits  = 4) { 
  require(igraph) #need igraph
  if (is.null(V(x)$name) == TRUE) {
    V(x)$name <- 1:vcount(x)
    names <- V(x)$name 
    }
  if (is.null(V(x)$name) == FALSE) {
    names <- V(x)$name 
    V(x)$name <- 1:vcount(x)
    }
  norm.max <- function(x) {return(x^2/max(x^2))} #max normalizing function
  norm.one <- function(x) {return(x^2)}
  norm.eig <- function(x, a) {return(x^2*a)}
  eig.x <- eigen(as_adjacency_matrix(x)) #eigenvector decomposition of adjacency matrix
  nc <- components(x)$no #number of components
  if (nc == 1) {s <- eig.x$vectors[, 1]; l <- eig.x$values[1]} #collecting status scores from first eigenvector in connected graph
  if (nc > 1) {s <- rowSums(eig.x$vectors[, 1:nc]); l <- sum(eig.x$values[1:nc])} #collecting status scores from separate eigenvectors in disconnected graph
  if (norm == "max") {s <- norm.max(s)} #normalizing so that maximum value is one
  if (norm == "one") {s <- norm.one(s)} #normalizing so that vector sums to one
  if (norm == "eig") {s <- norm.eig(s, l)} #normalizing so that vector sums to first eigenvalue
  names(s) <- V(x)$name #naming status vector using vector of node labels
  d <- rep(length(V(x)$name), 0) # initializing distinction vector to all zeros
  u <- rep(length(V(x)$name), 0) # initializing constraint vector to all zeros
  for (i in as.character(V(x)$name)) { #start looping across nodes in the graph
    j <- names(neighbors(x, i))  #vector of i's neighbors
    x.d <- delete_vertices(x, i) #node-deleted subgraph (minus i)
    nc <- components(x.d)$no #number of components of node-deleted subgraph
    eig.x.d <- eigen(as_adjacency_matrix(x.d)) # eigenvector decomposition of node-deleted subgraph
    c <- is_connected(x.d) #checking for connectedness of node-deleted subgraph
    e <- ecount(x.d) == 0 #checking for emptiness of node-deleted subgraph
    if (c == 1 & e == 0 & vcount(x.d) > 1 & norm == "max") {s.d <- norm.max(eig.x.d$vectors[, 1])} #connected non-empty node-deleted subgraph
    if (c == 1 & e == 0 & vcount(x.d) > 1 & norm == "one") {s.d <- norm.one(eig.x.d$vectors[, 1])} #connected non-empty node-deleted subgraph
    if (c == 1 & e == 0 & vcount(x.d) > 1 & norm == "eig") {s.d <- norm.eig(eig.x.d$vectors[, 1], eig.x.d$values[1])} #connected non-empty node-deleted subgraph
    if (c == 0 & e == 0 & vcount(x.d) > 1 & norm == "max") {s.d <- norm.max(rowSums(eig.x.d$vectors[, 1:nc]))} #disconnected non-empty node-deleted subgraph
    if (c == 0 & e == 0 & vcount(x.d) > 1 & norm == "one") {s.d <- norm.one(rowSums(eig.x.d$vectors[, 1:nc]))} #disconnected non-empty node-deleted subgraph
    if (c == 0 & e == 0 & vcount(x.d) > 1 & norm == "eig") {s.d <- norm.eig(rowSums(eig.x.d$vectors[, 1:nc]), sum(eig.x.d$values[1:nc]))} #disconnected non-empty node-deleted subgraph
    if (c == 1 & e == 0 & vcount(x.d) == 1) {s.d <- 0} #connected singleton node-deleted sub
    if (c == 0 & e == 1 & vcount(x.d) > 1) {s.d <- 0} #empty node-deleted subgraph
    names(s.d) <- names(j) #naming status vector in node-deleted subgraph
    u[i] <- mean(s.d) #i's average neighbor eigenvector centrality in node deleted subgraph
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
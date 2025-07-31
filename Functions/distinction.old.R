distinction <- function(x, norm = FALSE) { 
   library(igraph)
   V <- vcount(x)
   no.labels <- is.null(V(x)$name)
   if (no.labels == TRUE) {
      V(x)$name <- 1:V
      names <- V(x)$name 
      }
   if (no.labels == FALSE) {
      names <- V(x)$name 
      V(x)$name <- 1:V
      }
   norm.max <- function(x) {return(x/max(x))} #normalizing function
   A <- as.matrix(as_adjacency_matrix(x))
   nc <- components(x)$no #checking for disconnected graph
   if (nc > 1) {
      s <- rowSums(abs(eigen(A)$vectors[, 1:nc]))
      }
   else if (nc == 1) {
      s <- abs(eigen(A)$vectors[, 1])
      }
   if (norm == TRUE) {s <- norm.max(s)} #normalizing by maximum
   names(s) <- V(x)$name
   d <- rep(V, 0) # initializing distinction vector
   u <- rep(V, 0) # initializing constraint vector
   labs <- V(x)$name
   for (i in as.character(V(x)$name)) {
      j <- names(neighbors(x, i))
      x.d <- delete_vertices(x, i)
      c <- is_connected(x.d) #checking for connectedness of node-deleted subgraph
      e <- ecount(x.d) == 0 #checking for emptiness of node-deleted subgraph
      if (c == TRUE & e == FALSE) { #connected non-empty node-deleted graph
         A <- as.matrix(as_adjacency_matrix(x.d))
         s.a <- abs(eigen(A)$vectors[, 1])
         if (vcount(x.d) == 1) {
            s.a <- 0 
            }
         names(s.a) <- labs[-which(labs == i)]
         if (norm == TRUE) {s.a <- norm.max(s.a)} #normalizing by maximum
         s.a <- s.a[j]
         } #end main if
      else if (c == FALSE & e == TRUE) { #empty node-deleted graph
         s.a <- 0
         } #end first else 
      else if (c == FALSE & e == FALSE) { #disconnected non-empty node-deleted graph
         A <- as.matrix(as_adjacency_matrix(x.d))
         nc <- components(x.d)$no         
         s.a <- rowSums(abs(eigen(A)$vectors[, 1:nc]))
         names(s.a) <- labs[-which(labs == i)]
         if (norm == TRUE) {s.a <- norm.max(s.a)} #normalizing by maximum
         s.a <- s.a[j]
         } #end second else
      u[i] <- mean(s.a) #i's average neighbor eigenvector centrality in node deleted subgraph
      d[i] <- s[i] - u[i] #i's distinction centrality
      } #end i for loop
   d[is.na(d)] <- 0
   u[is.na(u)] <- 0
   scalar <- mean(u)/mean(s)
   sd <- (s*scalar) - u
   dat <- data.frame(d = d, sd = sd, s = s, u = u, scalar = scalar)
   dat <- round(dat, 4)
   dat <- data.frame(n = names, dat)
   rownames(dat) <- 1:nrow(dat)
   return(dat)
} #end function
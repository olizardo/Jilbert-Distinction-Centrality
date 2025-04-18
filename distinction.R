distinction <- function(x) { #distinction centrality function
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
   s <- eigen_centrality(x)$vector
   d <- rep(V, 0)
   u <- rep(V, 0)
   for (i in as.character(V(x)$name)) {
      j <- names(neighbors(x, i))
      x.d <- delete_vertices(x, i)
      c <- as.numeric(is_connected(x.d) == TRUE) #checking for connectedness
      e <- as.numeric(ecount(x.d) == 0) #checking for empty
      if (c == 1 & e == 0) { #connected non-empty graph
         s.a <- eigen_centrality(x.d)$vector[j]
         } #end if
      else if (c == 0 & e == 1) { #empty graph
         s.a <- 0
         } #end first else 
      else if (c == 0 & e == 0) { #disconnected non-empty graph
         C <- components(x.d)$membership
         names(C) <- V(x)$name[-as.numeric(i)]
         s.a <- rep(0, length(C))
         for (k in unique(C)) {
            sub.g <- subgraph(x.d, names(which(C == k)))
            if (vcount(sub.g) > 1) {
               s.a[which(C == k)] <- eigen_centrality(sub.g)$vector
               }
            }
         names(s.a) <- names(C)
         s.a <- s.a[j]
         } #end second else
      print(i)
      print(s.a)
      u[i] <- sum(s.a)/length(s.a) #i's average neighbor centrality in node deleted subgraph
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
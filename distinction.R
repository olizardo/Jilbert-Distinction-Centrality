distinction <- function(x, scale = FALSE) { #distinction centrality function
   library(igraph)
   if (is.null(V(x)$name) == TRUE) {
      V(x)$name <- 1:vcount(x)
      }
   s <- eigen_centrality(x)$vector
   d <- 0
   u <- rep(vcount(x), 0)
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
         } #end else 1
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
         } #end else 2
      u[i] <- sum(s.a)/length(s.a) #average neighbor centrality
      d[i] <- s[i] - u[i] #distinction centrality
   } #end i for loop
   print(s)
   print(u)
   d[is.na(d)] <- 0
   u[is.na(u)] <- 0
   scalar <- mean(u)/mean(s)
   if (scale == TRUE) {
      s <- s * scalar
      }
   d <- s - u
   sd <- (s*scalar) - u
   dat <- data.frame(n = V(x)$name, d = d, sd = sd, s = s, u = u, scalar = scalar)
   rownames(dat) <- 1:nrow(dat)
   return(round(dat, 4))
} #end function
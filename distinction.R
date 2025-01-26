distinction <- function(x) { #distinction centrality function
   if (is.null(V(x)$name) == TRUE) {
      V(x)$name <- 1:vcount(x)
      }
   s <- eigen_centrality(x)$vector
   d <- 0
   u <- 0
   for (i in 1:vcount(x)) {
      j <- names(neighbors(x, i))
      x.d <- x - i
      c <- as.numeric(is_connected(x.d) == TRUE) #checking for connectedness
      e <- as.numeric(ecount(x.d) != 0) #checking for empty
      if (c == 1 & e == 1) { #connected non-empty graph
         s.a <- eigen_centrality(x.d)$vector[j]
         } #end if
      else if (c == 0 & e == 0) { #empty graph
         s.a <- 0
         } #end else 1
      else if (c == 0 & e == 1) { #disconnected non-empty graph
         C <- components(x.d)$membership
         s.a <- eigen_centrality(subgraph(x.d, which(C == 1)))$vector
         for (k in 2:max(C)) {
            s.a <- c(s.a, eigen_centrality(subgraph(x.d, which(C == k)))$vector)
            }
         s.a <- s.a[j]
         } #end else 2
      u[i] <- sum(s.a) * 1/length(s.a) #average neighbor centrality
      d[i] <- s[i] - u[i] #distinction centrality
   } #end i for loop
   d[is.na(d)] <- 0
   u[is.na(u)] <- 0
   dat <- data.frame(n = V(x)$name, d = d, s = s, u = u)
   return(dat)
} #end function
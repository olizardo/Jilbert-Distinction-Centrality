   make.broker.trees <- function(n = 10) {
      g <- make_empty_graph(9, directed = FALSE)
      g <- g + edge(1,2)
      g <- g + edge(1,3)
      g <- g + edge(2,4)
      g <- g + edge(2,5)
      g <- g + edge(2,6)
      g <- g + edge(3,7)
      g <- g + edge(3,8)
      g <- g + edge(3,9)
      gl <- vector(mode = "list", length = n)
      gl[[1]] <- g
      a <- 10
      b <- 11
      c <- 12
      d <- 13
      for (i in 2:n) {
         g <- add_vertices(g, 4)
         g <- g + edge(1,a)
         g <- g + edge(a,b)
         g <- g + edge(a,c)
         g <- g + edge(a,d)
         gl[[i]] <- g
         a <- a + 4
         b <- b + 4
         c <- c + 4
         d <- d + 4
      }
   return(gl)
   }
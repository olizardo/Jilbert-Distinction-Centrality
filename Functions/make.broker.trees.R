   make.broker.trees <- function(n = 10) {
      library(igraph)
      g <- make_empty_graph(9, directed = FALSE) %>%
         add_edges(c(1,2, 1,3, 2,4, 2,5, 2,6, 3,7, 3,8, 3,9))
      
      gl <- vector(mode = "list", length = n)
      gl[[1]] <- g
      a <- 10
      b <- 11
      c <- 12
      d <- 13
      for (i in 2:n) {
         g <- add_vertices(g, 4) %>%
            add_edges(c(1,a, a,b, a,c, a,d))
         gl[[i]] <- g
         a <- a + 4
         b <- b + 4
         c <- c + 4
         d <- d + 4
      }
   return(gl)
   }
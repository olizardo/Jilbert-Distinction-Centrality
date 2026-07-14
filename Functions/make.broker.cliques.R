make.broker.cliques <- function(n = 10) {
   library(igraph)
   g1 <- make_full_graph(4, directed = FALSE)
   g2 <- make_full_graph(4, directed = FALSE)
   g3 <- disjoint_union(list(g1, g2)) %>%
      add_vertices(1) %>%
      add_edges(c(9,4, 9,5))
   
   gl <- vector(mode = "list", length = n)
   gl[[1]] <- g3
   a <- 10
   for (i in 2:n) {
      g3 <- g3 %du% make_full_graph(4, directed = FALSE) %>%
         add_edges(c(9,a))
      gl[[i]] <- g3
      a <- a + 4
      }
return(gl)
}
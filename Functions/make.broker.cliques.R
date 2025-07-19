make.broker.cliques <- function(n = 10) {
   g1 <- make_full_graph(4, directed = FALSE)
   g2 <- make_full_graph(4, directed = FALSE)
   g3 <- g1 + g2
   g3 <- add_vertices(g3, 1)
   g3 <- g3 + edge(9,4)
   g3 <- g3 + edge(9,5)
   gl <- vector(mode = "list", length = n)
   gl[[1]] <- g3
   a <- 10
   for (i in 2:n) {
      g3 <- g3 + make_full_graph(4, directed = FALSE)
      g3 <- g3 + edge(9,a)
      gl[[i]] <- g3
      a <- a + 4
      }
return(gl)
}
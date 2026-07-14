make.bonacich <- function() {
   library(igraph)
   # Bonacich (2007) regular network Fig. 5
   g1 <- make_empty_graph(10, directed = FALSE) %>%
     add_edges(c(1,2, 1,3, 1,9, 2,3, 2,10, 3,4, 4,5, 4,6, 5,10, 5,6, 6,7, 7,8, 7,9, 8,10, 8,9))
     
   g2 <- make_empty_graph(12, directed = FALSE) %>%
     add_edges(c(1,2, 1,6, 1,7, 2,3, 2,10, 3,4, 3,8, 4,5, 4,11, 5,6, 5,9, 6,12, 7,8, 7,9, 8,9, 10,11, 10,12, 11,12))
     
   return(list(g1 = g1, g2 = g2))
}
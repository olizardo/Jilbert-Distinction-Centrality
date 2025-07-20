gg2 <- function(x, lay.o = "kk") {
   n <- vcount(x)
   V(x)$name <- 1:n
   d <- distinction(x)[, 3]
   c <-  vector(mode = "character", length = length(d))
   c[which(d < -0.05)] <- "tan2"
   c[which(d > 0.05)] <- "blue"
   c[which(d >=-0.05 & d <= 0.05)] <- "purple"
   c[which(d == max(d))] <- "red"
   w <- ggraph(x, layout = lay.o) + 
   geom_edge_link(color = "steelblue") + theme_graph() +
   geom_node_point(size = 5, color = c) + 
   geom_node_text(aes(label = name), color = "white", size = 3) 
   return(w)
   }
gg <- function(x, lay.o = "kk", star = 1) {
   n <- vcount(x)
   V(x)$name <- 1:n
   c <- c(rep("tan2", n))
   c[star] <- "red"
   gk <- which(degree(x) == 4)
   gk <- gk[gk != star]
   c[gk] <- "blue"
   w <- ggraph(x, layout = lay.o) + 
   geom_edge_link(color = "steelblue") + theme_graph() +
   geom_node_point(size = 5, color = c) + 
   geom_node_text(aes(label = name), color = "white", size = 3) +
   ylim(-4, 3.5) + xlim(-4, 3.25) 
   return(w)
   }
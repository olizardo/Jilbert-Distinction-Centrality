plot.graph.norm <- function(x, l = "kk", vs = 16, ts = 8) { #plotting function
  library(ggraph)
  d <- distinction(x)[, 3]
  nc <- vector(mode = "character", length = length(d))
  nc[which(d < -0.05)] <- "tan2"
  nc[which(d > 0.05)] <- "blue"
  nc[which(d >=-0.05 & d <= 0.05)] <- "purple"
  nc[which(d == max(d))] <- "red"
  if (is.null(V(x)$name) == TRUE) {
      n <- 1:vcount(x)
      tc <- "white"
      s <- vs
      }
  if (is.null(V(x)$name) == FALSE) {
      n <- V(x)$name
      tc <- nc
      vs <- 0
      }
  p <- ggraph(x, layout = l)
  p <- p + geom_edge_link(color = "steelblue", edge_width = 0.5)
  p <- p + geom_node_point(aes(x = x, y = y), size = vs, color = nc) 
  p <- p + geom_node_text(aes(label = n), size = ts, color = tc, fontface = "bold")
  p <- p + theme_graph() 
return(p)
}
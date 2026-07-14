plot.graph.cat<- function(x, l = "kk", vs = 16, ts = 8, dist_data = NULL) { #plotting function
  library(ggraph)
  if (is.null(dist_data)) {
    d <- distinction(x)[, 3]
  } else {
    d <- dist_data[, 3]
  }
  cols <- paletteer::paletteer_d("RColorBrewer::Set1")
  nc <- factor(d, labels = cols[1:length(unique(d))])
  if (is.null(V(x)$name) == TRUE) {
      names_to_plot <- 1:vcount(x)
      tc <- "white"
      s <- vs
      }
  if (is.null(V(x)$name) == FALSE) {
      names_to_plot <- V(x)$name
      tc <- nc
      vs <- 0
      }
  p <- ggraph(x, layout = l)
  p <- p + geom_edge_link(color = "steelblue", edge_width = 0.5)
  p <- p + geom_node_point(aes(x = x, y = y), size = vs, color = nc) 
  p <- p + geom_node_text(aes(label = names_to_plot), size = ts, color = tc, fontface = "bold")
  p <- p + theme_graph() 
return(p)
}
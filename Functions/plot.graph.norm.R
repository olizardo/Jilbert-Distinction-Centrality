plot.graph.norm <- function(x, l = "kk", vs = 16, ts = 8, c = 0.05, n = "max", dist_data = NULL) { #plotting function
  library(ggraph)
  if (is.null(dist_data)) {
    d <- distinction(x, norm = n)[, 3]
  } else {
    d <- dist_data[, 3]
  }
  nc <- vector(mode = "character", length = length(d))
  nc[which(d < -c)] <- "tan2"
  nc[which(d > c)] <- "blue"
  nc[which(d >=-c & d <= c)] <- "purple"
  nc[which(d == max(d) & d >= c)] <- "red"
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
  p <- p + theme_graph() + coord_cartesian(clip = "off")
return(p)
}
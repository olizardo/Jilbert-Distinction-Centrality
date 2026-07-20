plot.graph.norm <- function(x, l = "kk", vs = 12, ts = 5, c = 0.05, n = "abm", dist_data = NULL) { #plotting function
  if (is.null(dist_data)) {
    d <- distinction(x, norm = n)$scd
  } else {
    d <- dist_data$scd
  }
  
  if (is.null(V(x)$name)) {
      names_to_plot <- as.character(1:vcount(x))
      V(x)$name <- names_to_plot
      is_real_network <- FALSE
  } else {
      names_to_plot <- V(x)$name
      # Heuristic: if names are just digits, it's a toy network. If characters, it's real (e.g. Medici)
      is_real_network <- !all(names_to_plot %in% as.character(1:1000))
  }
  
  V(x)$scd <- d
  
  p <- ggraph(x, layout = l) +
    geom_edge_link(color = "gray50", edge_width = 1)
  
  if (is_real_network) {
    # Use smaller sizes for real empirical networks to avoid overlapping
    vs_real <- vs * 0.5
    ts_real <- ts * 0.7
    p <- p + geom_node_point(aes(fill = scd), shape = 21, size = vs_real, color = "black") +
         geom_node_text(aes(label = name), color = "black", size = ts_real, fontface = "bold", repel = TRUE) +
         scale_fill_gradient2(low = "dodgerblue", mid = "gray90", high = "firebrick", name = "Scaled\nDist.")
  } else {
    p <- p + geom_node_point(aes(fill = scd), shape = 21, size = vs, color = "black") +
         geom_node_text(aes(label = name), color = "black", size = ts, fontface = "bold") +
         scale_fill_gradient2(low = "dodgerblue", mid = "gray90", high = "firebrick", name = "Scaled\nDist.")
  }
  
  p <- p + theme_graph() + 
       coord_cartesian(clip = "off") +
       theme(legend.position = "bottom", legend.title = element_text(face = "bold"))
       
  return(p)
}
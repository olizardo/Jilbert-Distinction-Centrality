   library(igraph)
   library(ggraph)
   library(here)
   library(networkdata)
   library(ggpubr)
   library(patchwork)
   library(dplyr)
   library(purrr)
   library(tibble)
   library(kableExtra)
   # Source functions
   invisible(lapply(c("distinction.R", "plot.graph.norm.R", "tab.cent.R"), 
          function(f) source(here("Functions", f))))
   source(here("Code", "make.estrada.R"))
   # Wrapper for toy graph processing
   process_toy <- function(x, file, lab, cap, lay = "kk") {
      dist_data <- distinction(x)
      ggsave(here("Plots", "Toys", file), 
             plot = plot.graph.norm(x, l = lay, dist_data = dist_data))
      tab.cent(dist_data, name = tools::file_path_sans_ext(file), 
               label = lab, caption = cap)
   }
   c3 <- make_full_graph(3, directed = FALSE) # triangle
   c4 <- make_full_graph(4, directed = FALSE) # 4-clique
   c5 <- make_full_graph(5, directed = FALSE) # 5-clique
   t3 <- make_tree(n = 4, children = 3, mode = "undirected") # 3-tree
   t2 <- make_tree(n = 3, children = 2, mode = "undirected") # 2-tree
   s7 <- make_star(7, mode = "undirected") # star graph
   c7 <- make_full_graph(7, directed = FALSE) # 7-clique
   r7 <- make_ring(7) # circle graph
   k7 <- make_graph("Zachary") # zachary karate club
   w7 <- make_wheel(7) # wheel graph
   p7 <- make_empty_graph(7) + path(1:7) # path graph
   sf4 <- make_full_graph(4) %du% make_full_graph(3) %>% 
  add_edges(c(4,5, 4,6, 4,7)) # structural fold (4)
   sf5 <- make_full_graph(5) %du% make_full_graph(4) %>% 
  add_edges(c(5,6, 5,7, 5,8, 5,9)) # structural fold (5)
   b3 <- add_vertices(c3 + c3, 1) + edge(7,1) + edge(7,4) # triangle broker
   b4 <- add_vertices(c4 + c4, 1) + edge(9,1) + edge(9,5) # clique broker
   ct <- r7 + edge(6,1) # circle graph plus triangle
   cl <- r7 + edge(7,4) # circle graph plus long tie
   st1 <- s7 + edge(2,3) # star graph plus one triangle
   st2 <- st1 + edge(5,6) # star graph plus two triangles
   t222 <- t2 + t2
   t222 <- add_vertices(t222, 1)
   t222 <- t222 + edge(7, 1)
   t222 <- t222 + edge(7, 4) # tree mediator two branch
   t232 <- t3 + t3
   t232 <- add_vertices(t232, 1)
   t232 <- t232 + edge(9, 1)
   t232 <- t232 + edge(9, 5) # tree mediator three branch
   t222l <- t222 + edge(6, 2) # tree mediator two branch plus long   
   t232l <- t232 + edge(7, 2) # tree mediator three branch plus long
   cw <- graph_from_literal(1-2-3-4-1, 4-5-6-7-4) %>% 
      delete_vertex_attr("name") # circle weld
   tc <- cw + edge(2, 4) + edge(4, 6) # two-connected
   tcl <- tc + edge(1, 7) # two-connected plus long tie
   k3 <- add_vertices(c3, 1) 
   k3 <- k3 + edge(1, 4) # kite graph
   r6 <- add_vertices(make_ring(6), 1)
   r6 <- r6 + edge(1, 7) # ring plus pendant
   c7p <- add_vertices(c7, 1)
   c7p <- c7p + edge(1, 8) # 7-clique plust pendant
   c10p <- add_vertices(make_full_graph(10, directed = FALSE), 1) + edge(1, 11)
   # Use igraph::union to overlay the unnamed indices directly
   h <- igraph::union(
     make_ring(10) %>% add_edges(c(1,3, 1,9, 1,4, 1,8, 1,5, 1,7, 1,6)),
     make_full_graph(5, directed = FALSE)
   ) %>% add_edges(c(3,10))
   h1 <- add_vertices(h, 1) + edge(1, 11)
   h2 <- add_vertices(h, 1) + edge(2, 11)
   h3 <- add_vertices(h, 1) + edge(8, 11)
   kr <- connect(make_ring(10), order = 2) # circular lattice of degree 4
   sw1 <- add_edges(kr, c(4,9)) # sw graph with one long tie
   sw2 <- add_edges(sw1, c(7,2)) # sw graph with two long ties
   sw3 <- add_edges(sw2, c(6,1)) # sw graph with three long ties
   krp <- add_vertices(kr, 1) + edge(1, 11) # circular lattice plus pendant
   sw1p <- add_vertices(sw1, 1) + edge(1, 11) # sw graph with one long tie and pendant
   sw2p <- add_vertices(sw2, 1) + edge(1, 11) # sw graph with two long ties and pendant
   sw3p <- add_vertices(sw3, 1) + edge(1, 11) # sw graph with three long ties and pendant
   
   # Load Estrada graphs
   estrada_graphs <- make.estrada()
   estrada_1a <- estrada_graphs$g1 # Fig 1(a) https://doi.org/10.1103/PhysRevE.71.056103
   estrada_1b <- estrada_graphs$g2 # Fig 1(b) https://doi.org/10.1103/PhysRevE.71.056103

   # A single central registry mapping graphs to their export parameters
   toy_configs <- tribble(
     ~obj,    ~file,                         ~label,             ~lay,     ~caption,
     s7,      "star.png",                    "star",             "kk",     "Distinction centrality scores for the star graph.",
     c7,      "clique.png",                  "clique",           "circle", "Distinction centrality scores for the clique graph.",
     r7,      "circle.png",                  "circle",           "circle", "Distinction centrality scores for the circle graph.",
     k7,      "kite.png",                    "kite",             "kk",     "Distinction centrality scores for the kite graph.",
     w7,      "wheel.png",                   "wheel",            "kk",     "Distinction centrality scores for the wheel graph.",
     p7,      "path.png",                    "path",             "kk",     "Distinction centrality scores for the path graph.",
     sf4,     "sf4.png",                     "sf4",              "kk",     "Distinction centrality scores for the structural fold graph (4-Clique).",
     sf5,     "sf5.png",                     "sf5",              "kk",     "Distinction centrality scores for the structural fold graph (5-Clique).",
     b3,      "broker3.png",                 "b3",               "kk",     "Distinction centrality scores for broker graph (3-clique).",
     b4,      "broker4.png",                 "b4",               "kk",     "Distinction centrality scores for broker graph (4-Clique).",
     ct,      "circle-triangle1.png",        "circletriangle",   "circle", "Distinction centrality scores for the circle graph with triangle.",
     cl,      "circle-long.png",             "circlelong",       "circle", "Distinction centrality scores for the circle graph with long tie.",
     st1,     "star-triangle1.png",          "star1",            "kk",     "Distinction centrality scores for the star graph with one triangle.",
     st2,     "star-triangle2.png",          "star2",            "kk",     "Distinction centrality scores for the star graph with two triangles.",
     c3,      "triangle.png",                "triangle",         "circle", "Distinction centrality scores for the triangle.",
     c4,      "4-clique.png",                "clique4",          "circle", "Distinction centrality scores for the 4-clique.",
     c5,      "5-clique.png",                "clique5",          "circle", "Distinction centrality scores for the 5-clique.",
     t3,      "3-tree.png",                  "tree3",            "kk",     "Distinction centrality scores for the 3-tree.",
     t2,      "2-tree.png",                  "tree2",            "kk",     "Distinction centrality scores for the 2-tree.",
     t222,    "tree-mediator-two-branch.png","treemediator2",    "kk",     "Distinction centrality scores for the tree mediator two branch.",
     t232,    "tree-mediator-three-branch.png","treemediator3",  "kk",     "Distinction centrality scores for the tree mediator three branch.",
     t222l,   "tree-mediator-two-branch-plus-long.png","treemediator2l","kk", "Distinction centrality scores for the tree mediator two branch plus long.",
     t232l,   "tree-mediator-three-branch-plus-long.png","treemediator3l","kk","Distinction centrality scores for the tree mediator three branch plus long.",
     cw,      "circle-weld.png",             "circleweld",       "kk",     "Distinction centrality scores for the circle weld.",
     tc,      "two-connected.png",           "twoconnected",     "kk",     "Distinction centrality scores for the two-connected.",
     tcl,     "two-connected-plus-long-tie.png","twoconnectedlong","kk",   "Distinction centrality scores for the two-connected plus long tie.",
     k3,      "kite-graph.png",              "kitegraph",        "kk",     "Distinction centrality scores for the kite graph.",
     r6,      "ring-plus-pendant.png",       "ringpendant",      "kk",     "Distinction centrality scores for the ring plus pendant.",
     c7p,     "7-clique-plus-pendant.png",   "clique7pendant",   "kk",     "Distinction centrality scores for the 7-clique plust pendant.",
     c10p,    "10-clique-plus-pendant.png",  "clique10pendant",  "kk",     "Distinction centrality scores for the 10-clique plus pendant.",
     h,       "union-ring-clique.png",       "unionringclique",  "kk",     "Distinction centrality scores for the union of ring and clique.",
     h1,      "union-plus-pendant-1.png",    "unionpendant1",    "kk",     "Distinction centrality scores for the union plus pendant 1.",
     h2,      "union-plus-pendant-2.png",    "unionpendant2",    "kk",     "Distinction centrality scores for the union plus pendant 2.",
     h3,      "union-plus-pendant-8.png",    "unionpendant8",    "kk",     "Distinction centrality scores for the union plus pendant 8.",
     kr,      "circular-lattice.png",        "circularlattice",  "circle", "Distinction centrality scores for the circular lattice of degree 4.",
     sw1,     "sw-graph-one-long-tie.png",   "sw1",              "circle", "Distinction centrality scores for the sw graph with one long tie.",
     sw2,     "sw-graph-two-long-ties.png",  "sw2",              "circle", "Distinction centrality scores for the sw graph with two long ties.",
     sw3,     "sw-graph-three-long-ties.png","sw3",              "circle", "Distinction centrality scores for the sw graph with three long ties.",
     krp,     "circular-lattice-plus-pendant.png","circularlatticependant","kk","Distinction centrality scores for the circular lattice plus pendant.",
     sw1p,    "sw-graph-one-long-tie-and-pendant.png","sw1p",    "kk",     "Distinction centrality scores for the sw graph with one long tie and pendant.",
     sw2p,    "sw-graph-two-long-ties-and-pendant.png","sw2p",   "kk",     "Distinction centrality scores for the sw graph with two long ties and pendant.",
     sw3p,    "sw-graph-three-long-ties-and-pendant.png","sw3p", "kk",     "Distinction centrality scores for the sw graph with three long ties and pendant.",
     estrada_1a, "estrada-1a.png",                 "estrada1a",  "kk",     "Distinction centrality scores for the Estrada Fig 1(a) graph.",
     estrada_1b, "estrada-1b.png",                 "estrada1b",  "kk",     "Distinction centrality scores for the Estrada Fig 1(b) graph."
   )

   # Process all graphs programmatically
   pwalk(toy_configs, function(obj, file, label, lay, caption) {
     process_toy(obj, file, label, caption, lay)
   })

   medici1 <- as.matrix(read.csv(here("Data", "MediciEdgeList.csv")))
   medici1 <- graph_from_edgelist(medici1, directed = FALSE)
   medici2 <- as.matrix(read.csv(here("Data", "MediciEdgeListMarriageCut.csv")))
   medici2 <- graph_from_edgelist(medici2, directed = FALSE)
   nomedici <- as.matrix(read.csv(here("Data", "NoMediciEdgeList.csv")))
   nomedici <- graph_from_edgelist(nomedici, directed = FALSE)

library(igraph)
library(ggplot2)
library(ggraph)
library(here)
library(dplyr)
library(purrr)
library(kableExtra)

source(here("Functions", "distinction.R"))
source(here("Functions", "plot.graph.norm.R"))
source(here("Functions", "tab.cent.R"))

# Redefine tab.cent to include latex_options = "scale_down" and hold_position if needed
tab.cent <- function(x, 
      d = 3, 
      name = "name", 
      caption = "", 
      label = "label"
      ) { 
  
  x_grouped <- x %>%
    group_by(scd, s, u, scalar) %>%
    summarise(Nodes = paste(n, collapse = ", "), .groups = "drop") %>%
    select(Nodes, scd, s, u, scalar) %>%
    arrange(desc(scd))
  
  t <- kbl(x_grouped, 
        format = "latex", booktabs = TRUE, linesep = "",
        digits = d, row.names = FALSE,
        col.names = c("Nodes", "$\\beta_i$", "$s_i$", "$\\kappa_i$", "$\\alpha$"),
        caption = caption, label = label, escape = FALSE
        ) %>% 
        kable_styling(latex_options = c("scale_down", "hold_position")) %>% 
        save_kable(file = here("Tabs", paste(name, ".tex", sep = "")))
}

get_lcc <- function(g) {
  comp <- components(g)
  largest <- which.max(comp$csize)
  induced_subgraph(g, V(g)[comp$membership == largest])
}

medici1 <- as.matrix(read.csv(here("Data", "MediciEdgeList.csv")))
medici1 <- get_lcc(graph_from_edgelist(medici1, directed = FALSE))

medici2 <- as.matrix(read.csv(here("Data", "MediciEdgeListMarriageCut.csv")))
medici2 <- get_lcc(graph_from_edgelist(medici2, directed = FALSE))

nomedici <- as.matrix(read.csv(here("Data", "NoMediciEdgeList.csv")))
nomedici <- get_lcc(graph_from_edgelist(nomedici, directed = FALSE))

process_toy <- function(x, file, lab, cap, lay = "kk") {
   dist_data <- distinction(x)
   ggsave(here("Plots", file), 
          plot = plot.graph.norm(x, l = lay, dist_data = dist_data), width=7, height=7)
   tab.cent(dist_data, name = tools::file_path_sans_ext(file), 
            label = lab, caption = cap)
}

process_toy(medici1, "medici-full.png", "medicifull", "Distinction centrality scores for the full Medici network.", "kk")
process_toy(medici2, "medici-marriage-cut.png", "medicimarriagecut", "Distinction centrality scores for the Medici network with marriage cut.", "kk")
process_toy(nomedici, "medici-removed.png", "mediciremoved", "Distinction centrality scores for the network with the Medici family removed.", "kk")

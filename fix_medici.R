library(igraph)
library(ggplot2)
library(ggraph)
library(here)
library(dplyr)
library(purrr)
library(kableExtra)

# Source functions
invisible(lapply(c("distinction.R", "plot.graph.norm.R", "tab.cent.R"), 
       function(f) source(here("Functions", f))))

get_lcc <- function(g) {
  comp <- components(g)
  largest <- which.max(comp$csize)
  induced_subgraph(g, V(g)[comp$membership == largest])
}

# Load Medici Data
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

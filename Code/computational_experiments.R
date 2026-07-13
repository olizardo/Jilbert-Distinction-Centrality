library(igraph)
library(ggplot2)
library(here)
library(patchwork)
library(dplyr)

source(here("Functions", "distinction.R"))
source(here("Functions", "plot.graph.norm.R"))

# ====================================================================
# Star Graph Computational Experiments
# ====================================================================
make.stars <- function(n = 30) {
    gl <- vector(mode = "list", length = n)
    gl[[1]] <- make_star(3, mode = "undirected") 
    for (k in 2:n) {
        gl[[k]] <- make_star(k+2, mode = "undirected") 
        }
    return(gl)
}

b <- make.stars(33)
c <- lapply(b, distinction)
ds.s <- sapply(c, function(x) {x[1, 3]})
ds.p <- sapply(c, function(x) {x[2, 3]})
d.s <- sapply(c, function(x) {x[1, 2]})
d.p <- sapply(c, function(x) {x[2, 2]})
dat <- data.frame(d = c(ds.s, ds.p, d.s, d.p), 
      n = c(1:length(d.s), 1:length(d.s), 1:length(d.s), 1:length(d.s)),
      g = c(rep(1, length(d.s)), rep(2, length(d.s)), 
      rep(3, length(d.s)), rep(4, length(d.s)))) |> 
      mutate(g = factor(g, labels = c(
                        "Scaled Distinction (Central Node)", 
                        "Scaled Distinction (Peripheral Nodes)", 
                        "Non-scaled Distinction (Central Node)",
                        "Non-scaled Distinction (Peripheral Nodes)"))) |> 
      mutate(n = n + 2)

p <- ggplot(dat, aes(n, d, group = g, color = as.factor(g))) +
    geom_point(size = 3) +
    geom_line(linewidth = 0.75) +
    geom_hline(yintercept = 0, color = "red", linetype = 2) +
    labs(y = "", x = "Star Size") +
    scale_x_continuous(breaks = seq(3, length(d.s), by = 3)) +
    scale_color_manual(values = c("red", "blue", "tan2", "purple")) +
    theme_minimal() +
    theme(axis.text = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          legend.text = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom"
          ) +
    guides(color = guide_legend(nrow = 4))

ggsave(here("Plots", "star-roles-by-size.png"), plot = p, width = 7, height = 8, units = "in")


# ====================================================================
# Connected Caves Computational Experiments
# ====================================================================
source(here("Functions", "connect.caves.R"))

set.seed(456)
b <- connect.caves(c = 4, nc = 4, n = 24)
p.list <- lapply(b, plot.graph.norm, l = "auto")
for (i in 1:length(p.list)) {
    p.list[[i]] <- p.list[[i]] + ggtitle(paste("N. Long Ties =", i-1)) +
        theme(plot.title = element_text(size = 11))
}
wrap_plots(p.list[c(1:4)], nrow = 2)
ggsave(here("Plots", "connected-caves-examples1.png"), width = 10, height = 10, units = "in")
wrap_plots(p.list[c(5:8)], nrow = 2)
ggsave(here("Plots", "connected-caves-examples2.png"), width = 10, height = 10, units = "in")
wrap_plots(p.list[c(9:12)], nrow = 2)
ggsave(here("Plots", "connected-caves-examples3.png"), width = 10, height = 10, units = "in")
wrap_plots(p.list[c(13:16)], nrow = 2)
ggsave(here("Plots", "connected-caves-examples4.png"), width = 10, height = 10, units = "in")


# ====================================================================
# Broker Computational Experiments
# ====================================================================

# Trees
source(here("Functions", "make.broker.trees.R"))
b <- make.broker.trees(30)
p.list <- lapply(b, plot.graph.norm, l = "auto")
for (i in 1:length(p.list)) {
    p.list[[i]] <- p.list[[i]] + ggtitle(paste("N. Branches =", i+1)) +
        theme(plot.title = element_text(size = 11))
}
wrap_plots(p.list[c(1:6)], nrow = 2)
ggsave(here("Plots", "tree-examples.png"), width = 12, height = 8, units = "in")

# Tree Roles Chart
c <- lapply(b, distinction, norm = "max")
d.s <- sapply(c, function(x) {x[1, 3]})
d.b <- sapply(c, function(x) {x[2, 3]})
d.p <- sapply(c, function(x) {x[nrow(x), 3]})
dat <- data.frame(d = c(d.s, d.b, d.p), 
      n = c(1:length(d.s)+1, 1:length(d.s)+1, 1:length(d.s)+1),
      g = c(rep(1, length(d.s)), rep(2, length(d.s)), 
      rep(3, length(d.s)))) |> 
      mutate(g = factor(g, labels = c("Star", "Gatekeeper", "Peripheral")))

p <- ggplot(dat, aes(n, d, group = g, color = as.factor(g))) +
    geom_point(size = 3) +
    geom_line(linewidth = 0.75) +
    geom_hline(yintercept = 0, color = "red", linetype = 2) +
    labs(y = "", x = "Number of Branches") +
    scale_x_continuous(breaks = seq(2, length(d.s), by = 2)) +
    scale_color_manual(values = c("red", "blue", "tan2")) +
    theme_minimal()
p1 <- p + theme(axis.text = element_text(size = 14, face = "bold"),
                axis.title = element_text(size = 16, face = "bold"),
                legend.text = element_text(size = 16, face = "bold"),
                legend.title = element_blank(),
                legend.position = "top"
                ) + ggtitle("Abs. Value Norm")

c <- lapply(b, distinction, norm = "abm")
d.s <- sapply(c, function(x) {x[1, 3]})
d.b <- sapply(c, function(x) {x[2, 3]})
d.p <- sapply(c, function(x) {x[nrow(x), 3]})
dat <- data.frame(d = c(d.s, d.b, d.p), 
      n = c(1:length(d.s)+1, 1:length(d.s)+1, 1:length(d.s)+1),
      g = c(rep(1, length(d.s)), rep(2, length(d.s)), 
      rep(3, length(d.s)))) |> 
      mutate(g = factor(g, labels = c("Star", "Gatekeeper", "Peripheral")))

p <- ggplot(dat, aes(n, d, group = g, color = as.factor(g))) +
    geom_point(size = 3) +
    geom_line(linewidth = 0.75) +
    geom_hline(yintercept = 0, color = "red", linetype = 2) +
    labs(y = "", x = "Number of Branches") +
    scale_x_continuous(breaks = seq(2, length(d.s), by = 2)) +
    scale_color_manual(values = c("red", "blue", "tan2")) +
    theme_minimal()
p2 <- p + theme(axis.text = element_text(size = 14, face = "bold"),
                axis.title = element_text(size = 16, face = "bold"),
                legend.text = element_text(size = 16, face = "bold"),
                legend.title = element_blank(),
                legend.position = "top"
                ) + ggtitle("Squared Norm")

p1 + p2
ggsave(here("Plots", "broker-tree-roles-by-nbranches.png"), width = 10, height = 7, units = "in")

# Cliques
source(here("Functions", "make.broker.cliques.R"))
b <- make.broker.cliques(30)
p.list <- lapply(b, plot.graph.norm, l = "auto")
for (i in 1:length(p.list)) {
    p.list[[i]] <- p.list[[i]] + ggtitle(paste("N. Branches =", i+1)) +
        theme(plot.title = element_text(size = 11))
    }
wrap_plots(p.list[c(1:6)], nrow = 2)
ggsave(here("Plots", "clique-examples.png"), width = 12, height = 8, units = "in")

c <- lapply(b, distinction, norm = "abm")
d.s <- sapply(c, function(x) {x[9, 3]})
d.b <- sapply(c, function(x) {x[4, 3]})
d.p <- sapply(c, function(x) {x[1, 3]})
dat <- data.frame(d = c(d.s, d.b, d.p), 
      n = c(1:length(d.s)+1, 1:length(d.s)+1, 1:length(d.s)+1),
      g = c(rep(1, length(d.s)), rep(2, length(d.s)), 
      rep(3, length(d.s)))) |> 
      mutate(g = factor(g, labels = c("Star", "Gatekeeper", "Peripheral")))

p <- ggplot(dat, aes(n, d, group = g, color = as.factor(g))) +
    geom_point(size = 3) +
    geom_line(linewidth = 0.75) +
    geom_hline(yintercept = 0, color = "red", linetype = 2) +
    labs(y = "", x = "Number of Branches") +
    scale_x_continuous(breaks = seq(2, length(d.s), by = 2)) +
    scale_color_manual(values = c("red", "blue", "tan2")) +
    theme_minimal() +
    theme(axis.text = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          legend.text = element_text(size = 16, face = "bold"),
          legend.title = element_blank(),
          legend.position = "top"
          )
p
ggsave(here("Plots", "broker-cliques-roles-by-nbranches.png"), width = 7, height = 7, units = "in")

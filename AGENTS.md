# Project Memory - Distinction Centrality Analysis

## Project Overview

This project implements and analyzes **Distinction Centrality** ($\beta_i$), a social network metric inspired by Pierre Bourdieu's theory of agonistic competition in social fields. It trades off status (eigenvector centrality, $s_i$) against constraint (average status of neighbors in the node-deleted subgraph, $\kappa_i$):

$$\beta_i = \alpha s_i - \kappa_i, \qquad \alpha = \frac{\bar{\kappa}}{\bar{s}}$$

---

## Data Catalog

All data files are in `Data/` and are undirected edge lists with columns `a` and `b`.

| File | Rows | Description |
| :--- | :--- | :--- |
| `MediciEdgeList.csv` | 42 | Padgett & Ansell (1993) Medici family network. |
| `MediciEdgeListMarriageCut.csv` | 41 | Medici network with one marriage edge removed. |
| `NoMediciEdgeList.csv` | 36 | Florentine network without the Medici family. |
| `karate.csv` | 153 | Zachary karate club: 34 nodes, 78 unique undirected edges (see note). |
| `karatehi9_2-newest.csv` | 36 | Karate club split, "hi" faction (not yet documented). |
| `karateoff9_2-newest.csv` | 34 | Karate club split, "off" faction (not yet documented). |
| `poli1.csv` to `poli4.csv` | 33, 36, 39, 41 | Political network experiment, steps 1 to 4 (added edges). |
| `searchmodeltest.csv` | 18 | Network for testing search model dynamics. |

Note: `karate.csv` has 153 rows but only 78 unique undirected pairs and no exact duplicate rows. Check whether it lists both directions or a different edge set before using it for edge counts.

---

## Code Structure

**`Functions/`** holds the core metrics and plotting code:

- `distinction.R`: computes $\beta_i$, scaled distinction (`scd`), status, constraint, and $\alpha$ for an `igraph` object.
- `plot.graph.norm.R`: `ggraph` plot with nodes colored by scaled distinction (`dodgerblue` to `gray90` to `firebrick`).
- `plot.graph.cat.R`: categorical visualization of distinction scores.
- `tab.cent.R`: formats centrality scores with `kableExtra` and writes LaTeX tables to `Tabs/`.
- `diss.status.R`: status using a similarity-discounted adjacency matrix.
- `make.estrada.R`, `make-bonacich.R`: example graphs from Estrada (2005) and Bonacich (2007).
- `connect.caves.R`, `make.broker.cliques.R`, `make.broker.trees.R`: caveman graphs and brokerage structures.

**`Code/`** holds analysis and figure scripts:

- `make_plots_tables.R`: the toy-graph registry (about 42 graphs) and export parameters. This is the source of truth for toy graph definitions.
- `computational_experiments.R`: connected-caves experiments.
- `plot_constraint_vs_order_circle.R`, `plot_small_world_examples.R`, `plot_small_world_evolution.R`, `plot_span_of_control.R`, `plot_tree_comparisons.R`, `plot_rooted_tree.R`: figure scripts for the manuscript sections.
- `analysis-old.R`, `jilbert.R`: older analysis scripts. Check before reusing.

---

## Key Files and Outputs

- `manuscript.tex` / `manuscript.pdf`: main paper draft. `appendix_proofs.tex` and `estrada_section.tex` are included parts.
- `Website/`: Quarto site (`_quarto.yml` is here, not at the root). Pages: `index.qmd`, `functions.qmd`, `examples.qmd`, `small-world.qmd`, `span-of-control.qmd`.
- `distinction-pagerank-report.qmd` / `.html`: distinction vs. PageRank report at the project root. Self-contained apart from `Data/` and `Functions/distinction.R` / `Functions/plot.graph.norm.R`. **Rebuilt 2026-11-08** (the previous `.qmd` had been emptied to 0 bytes and rendered a blank HTML page; it was untracked, so the old content was unrecoverable), revised 2026-11-09. Sections: guiding question, empirical networks (full Medici, Medici largest component, and karate in one table), toy-graph registry (13 graphs with Pearson-vs-Kendall scatter), Medici node-level rank-gap table and gap-colored network plot (both restricted to the largest component of 28 families), disagreement cases (b3, b4, sf5, krp as a patchwork 2×2 figure with panel tags), and interpretation. Render with the Quarto CLI at `/usr/share/positron/resources/app/quarto/bin/quarto`.
- `Tabs/`: LaTeX table exports.
- `Plots/`: generated figures, including `distinction-pagerank-inversion.png`.

---

## Quarto Rendering Notes

`Website/` renders slowly when pages use custom HTML layouts or raw `cat()` output. Keep outputs standard and minimal, and use `cache: true` for heavy chunks.

---

## Distinction vs. PageRank Results (2026-11-08, revised 2026-11-09)

Correlations between scaled distinction and undirected igraph PageRank:

- Medici: Pearson 0.576, Spearman 0.855, Kendall 0.681. Restricted to the largest connected component (28 families): Pearson 0.892, Spearman 0.958, Kendall 0.838 — the full-graph weakness was driven by disconnected dyads/triangles with local jackknife normalizations.
- Karate club (as read via `unique()` on `Data/karate.csv`, 34 nodes): Pearson 0.948, Spearman 0.980, Kendall 0.907 (recomputed live 2026-11-09; older notes had 0.947/0.989/0.939 from a slightly different edge handling).
- Toy graphs (42): agreement is high for trees, circles with long ties, star plus triangles, and small-world variants (Kendall about 0.45 to 1.00). The structural fold (5-clique) has Pearson -1.00. The two clique-broker graphs have negative Kendall's tau (-0.14 for b3, -0.20 for b4). The lattice with a pendant (`krp`) has Kendall near 0. Cliques, circles, and the regular lattice return `NA` because distinction is constant.
- In the broker graphs, the broker nodes have low PageRank and the lowest distinction. They are not PageRank's top nodes. Only the structural fold has a bridge node (5) with both the highest PageRank and the lowest distinction.
- Medici node-level rank gaps (2026-11-08, largest connected component of 28 families; the report restricts to it because small components get local jackknife normalizations and manufacture spurious gaps): largest gap is Da Uzzano (distinction 21st, PageRank 15th, gap 6). Medici: PageRank 3rd, distinction 7th (gap 4) — its six ties bind a neighborhood that survives deletion. Ginori +5; Valori -5 (largest negative). The elite core agrees almost fully (Peruzzi and Albizzi top-2 under both). On the FULL 33-node graph the gaps were much larger (Davanzati/Orlandini -12, Medici -7, Panciatichi +10) but were driven by dyad/triangle components.

---

## Environment Setup

The project uses renv. `renv.lock` (R 4.5.3, renv 1.3.1) records all packages, and `renv/library/` holds the project library. Restore with:

```r
install.packages("renv")
renv::restore()
```

- `networkdata` is GitHub-only: `renv::install("schochastics/networkdata")`.
- `igraph` needs the system library `libglpk-dev` (requires an administrator).
- Install packages through `Rscript --no-save --no-restore -e '...'`, not the Positron console, which can hang on prompts.

Analysis scripts typically load `igraph`, `ggraph`, `here`, `dplyr`, `kableExtra`, `ggpubr`, and `patchwork`.

For LaTeX, TinyTeX lives in `~/.TinyTeX` with binaries in `~/bin/`. Do not compile locally when the project is connected to Overleaf.

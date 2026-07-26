lines <- readLines("manuscript.tex")

# Replace "Figure 3 \textit{Model (a)} and Table 4" with "Figure~\ref{fig:circletriangle} and Table~\ref{tab:circletriangle}"
lines <- gsub("Figure 3 \\\\textit\\{Model \\(a\\)\\} and Table 4", "Figure~\\\\ref{fig:circletriangle} and Table~\\\\ref{tab:circletriangle}", lines)

# Replace "Figure 3 \textit{Model (b)} and Table 5" with "Figure~\ref{fig:circlelong} and Table~\ref{tab:circlelong}"
lines <- gsub("Figure 3 \\\\textit\\{Model \\(b\\)\\} and Table 5", "Figure~\\\\ref{fig:circlelong} and Table~\\\\ref{tab:circlelong}", lines)

# Replace "Figure 3 \textit{Model (a)}" alone
lines <- gsub("Figure 3 \\\\textit\\{Model \\(a\\)\\}", "Figure~\\\\ref{fig:circletriangle}", lines)

# Replace \textit{Model (b)} presents ... Table 2
lines <- gsub("\\\\textit\\{Model \\(b\\)\\} presents an idealized depiction of a structural fold \\\\citep\\{vedres2010structural\\}. The resulting measurement scores are in Table 2.", "Figure~\\\\ref{fig:sf4} presents an idealized depiction of a structural fold \\\\citep{vedres2010structural}. The resulting measurement scores are in Table~\\\\ref{tab:sf4}.", lines)

# Replace \textit{Model (c)} presents ... Table 3
lines <- gsub("\\\\textit\\{Model \\(c\\)\\} presents a graph that is 2-connected in the absence of node 1. Each subgraph is one connection short of forming complete subgraphs in the absence of Node 1. Compared to the structural fold, connections 3-2 and 6-7 have been removed. Measurement results are in Table 3.", "Figure~\\\\ref{fig:twoconnected} presents a graph that is 2-connected in the absence of node 1. Each subgraph is one connection short of forming complete subgraphs in the absence of Node 1. Compared to the structural fold, connections 3-2 and 6-7 have been removed. Measurement results are in Table~\\\\ref{tab:twoconnected}.", lines)

# Replace \textit{Model (d)}
lines <- gsub("\\\\textit\\{Model \\(d\\)\\} is the same 2-connected graph with the addition of a long tie", "Figure~\\\\ref{fig:circleweld} is the same 2-connected graph with the addition of a long tie", lines)

# Insert the tables after the figure
idx <- grep("% \\\\input\\{Tabs/Figure4\\}", lines)
if(length(idx) > 0) {
  insert_str <- c(
    "\\input{Tabs/circle-triangle1.tex}",
    "\\input{Tabs/circle-long.tex}",
    "\\input{Tabs/star-triangle1.tex}",
    "\\input{Tabs/wheel.tex}",
    "\\input{Tabs/sf4.tex}",
    "\\input{Tabs/two-connected.tex}",
    "\\input{Tabs/circle-weld.tex}",
    "\\input{Tabs/broker4.tex}",
    "\\input{Tabs/tree-mediator-three-branch.tex}"
  )
  lines <- c(lines[1:(idx-1)], insert_str, lines[(idx+1):length(lines)])
}

writeLines(lines, "manuscript.tex")

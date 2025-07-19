connect.caves <- function(c = 4, nc = 4, n = 12) {
    make.cave.list <- function(c) {
      caves <- list() 
      i <- 1
      for (k in 1:nc) {
        j <- i + (c - 1)
        caves[[k]] <- c(i:j)
        i <- i + c
      }
    return(caves)
    } # end make.cave.list
    caves <- make.cave.list(c)
    make.caves <- function(c, nc) {
      g <- make_full_graph(c, directed = FALSE)
      for (k in 2:nc) {
        g <- g + make_full_graph(c, directed = FALSE)
        }
      return(g) 
      } # end make.caves
    gl <- vector(mode = "list", length = n)
    g <- make.caves(c, nc)
    gl[[1]] <- g
    for (k in 2:n) {
      ca <- sample(1:c, 2)
      ca1 <- caves[[ca[1]]]
      ca2 <- caves[[ca[2]]]
      g <- add_edges(g, c(sample(ca1, 1), sample(ca2, 1)))
      gl[[k]] <- g
      }
    return(gl)
}
connect.caves <- function(c = 4, nc = 4, n = 12) {
    make.cave.list <- function(a, b) {
      caves <- list() 
      i <- 1
      for (k in 1:b) {
        j <- i + (a - 1)
        caves[[k]] <- c(i:j)
        i <- i + a
      }
    return(caves)
    } # end make.cave.list
    caves <- make.cave.list(c, nc)
    make.caves <- function(a, b) {
      g <- make_full_graph(a, directed = FALSE)
      for (k in 2:b) {
        g <- g + make_full_graph(a, directed = FALSE)
        }
      return(g) 
      } # end make.caves
    g <- make.caves(c, nc)
    gl <- vector(mode = "list", length = n)
    gl[[1]] <- g
    for (k in 2:n) {
      ca <- sample(1:nc, 2)
      ca1 <- caves[[ca[1]]]
      ca2 <- caves[[ca[2]]]
      g <- add_edges(g, c(sample(ca1, 1), sample(ca2, 1)))
      gl[[k]] <- g
      }
    return(gl)
}
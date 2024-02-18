createGraphFromAdjacencyMatrix <- function(netobj) {
  graph_from_adjacency_matrix(
    abs(netobj$graph),
    mode = "lower",
    weighted = TRUE,
    diag = FALSE,
    add.colnames = TRUE,
    add.rownames = FALSE
  )
}

topo_sort <- function(adj_matrix) {

  stopifnot(
    is.matrix(adj_matrix), nrow(adj_matrix) == ncol(adj_matrix),
    identical(rownames(adj_matrix), colnames(adj_matrix)),
    is.integer(adj_matrix), all(adj_matrix %in% c(0L, 1L))
  )

  num_nodes <- nrow(adj_matrix)

  if (!num_nodes) {
    return(character())
  }

  dfs <- local({

    visited <- rep(FALSE, num_nodes)
    on_stack <- rep(FALSE, num_nodes)

    function(node, adj, stack) {

      if (on_stack[node]) {
        stop("The graph contains a cycle and is not a DAG.")
      }

      if (visited[node]) {
        return(stack)
      }

      visited[node] <<- TRUE
      on_stack[node] <<- TRUE

      for (neighbor in which(adj[node, ] == 1)) {
        stack <- Recall(neighbor, adj, stack)
      }

      on_stack[node] <<- FALSE

      c(node, stack)
    }
  })

  stack <- integer()

  for (node in seq_len(num_nodes)) {
    stack <- dfs(node, adj_matrix, stack)
  }

  rownames(adj_matrix)[stack]
}

as_adjacency_matrix <- function(from, to, nodes = unique(c(from, to))) {

  stopifnot(
    all(from %in% nodes), all(to %in% nodes), anyDuplicated(nodes) == 0L
  )

  n <- length(nodes)

  res <- matrix(0L, n, n, dimnames = list(nodes, nodes))
  res[cbind(from, to)] <- 1L

  res
}

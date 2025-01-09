topo_sort <- function(vertices, edges) {

  stopifnot(
    is.character(vertices), is.data.frame(edges),
    setequal(colnames(edges), c("from", "to")),
    all(edges[["from"]] %in% vertices), all(edges[["to"]] %in% vertices)
  )

  num_nodes <- length(vertices)

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

  adj_matrix <- matrix(0L, num_nodes, num_nodes,
                       dimnames = list(vertices, vertices))
  adj_matrix[as.matrix(edges[, c("from", "to")])] <- 1L

  stack <- integer()

  for (node in seq_len(num_nodes)) {
    stack <- dfs(node, adj_matrix, stack)
  }

  vertices[stack]
}

#' Generics for UI generation
#'
#' Generics for creating fields and block UI containers.
#'
#' @param x Object for which to generate a UI container
#' @param id (Optional) parent namespace
#' @param ... Generic consistency
#'
#' @export
block_ui <- function(x, id = NULL, ...) {
  UseMethod("block_ui")
}

#' @rdname block_ui
#' @export
block_ui.block <- function(x, id = NULL, ...) {
  tagList(
    expr_ui(x, block_ns(x, namespace = id), ...),
    DT::dataTableOutput(block_ns(x, "result", namespace = id))
  )
}

#' @param ns A function for constructing shiny module namespaces
#' @rdname block_ui
#' @export
expr_ui <- function(x, ns = block_ns(x), ...) {
  UseMethod("expr_ui")
}

#' @rdname block_ui
#' @export
expr_ui.block <- function(x, ns = block_ns(x), ...) {
  do.call(
    block_expr_ui(x),
    list(ns = ns, ...)
  )
}

#' @param result Reactive block result
#' @rdname block_ui
#' @export
block_output <- function(x, result) {
  UseMethod("block_output")
}

#' @rdname block_ui
#' @export
block_output.block <- function(x, result) {
  DT::renderDT(
    DT::datatable(
      result(),
      selection = "none",
      options = list(
        pageLength = 5L,
        processing = FALSE
      )
    ),
    server = TRUE
  )
}

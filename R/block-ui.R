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

#' @param ns A function for constructing shiny module namespaces
#' @rdname block_ui
#' @export
expr_ui <- function(x, ns = block_ns(x), ...) {
  UseMethod("expr_ui")
}

#' @rdname block_ui
#' @export
expr_ui.block <- function(x, ns = block_ns(x), ...) {
  fun <- block_expr_ui(x)
  eval(fun(ns), envir = list(...), enclos = environment(fun))
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

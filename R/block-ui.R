#' Generics for UI generation
#'
#' Generics for creating fields and block UI containers.
#'
#' @param x Object for which to generate a UI container
#' @param ... Generic consistency
#'
#' @export
block_ui <- function(x, ...) {
  UseMethod("block_ui")
}

#' @param id (Optional) parent namespace
#' @rdname block_ui
#' @export
block_ui.block <- function(x, id = "block", ...) {
  tagList(
    expr_ui(x, id, ...),
    DT::dataTableOutput(NS(id, "result"))
  )
}

#' @rdname block_ui
#' @export
expr_ui <- function(x, id = "block", ...) {
  UseMethod("expr_ui")
}

#' @rdname block_ui
#' @export
expr_ui.block <- function(x, id = "block", ...) {

  if (...length()) {
    stop(
      "Unknown arguments ", paste_enum(...names()), " in call to `expr_ui()`."
    )
  }

  do.call(block_expr_ui(x), list(id = NS(id, "expr")))
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

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

#' @rdname block_ui
#' @export
fields_ui <- function(x, ...) {
  UseMethod("fields_ui")
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

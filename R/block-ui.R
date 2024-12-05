#' Generics for UI generation
#'
#' Generics for creating fields and block UI containers.
#'
#' @param x Object for which to generate a UI container.
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

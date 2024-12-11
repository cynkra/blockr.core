#' Generics for server generation
#'
#' Calls shiny modules for the given element (block, fields).
#'
#' @param x Object for which to generate a [moduleServer()]
#' @param data Data input (list of reactive values)
#' @param ... Generic consistency
#'
#' @export
block_server <- function(x, data = list(), ...) {
  UseMethod("block_server")
}

#' @rdname block_server
#' @export
expr_server <- function(x, data = list(), ...) {
  UseMethod("expr_server")
}

#' @rdname block_server
#' @export
expr_server.block <- function(x, ...) {
  x[["expr_server"]](...)
}


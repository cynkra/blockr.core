#' Generics for server generation
#'
#' Calls shiny modules for the given element (block, fields).
#'
#' @param x Object for which to generate a [moduleServer()]
#' @param input Data input (list of reactive values)
#' @param ... Generic consistency
#'
#' @export
block_server <- function(x, input = list(), ...) {
  UseMethod("block_server")
}

#' @rdname block_server
#' @export
fields_server <- function(x, input = list(), ...) {
  UseMethod("fields_server")
}

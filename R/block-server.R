#' Generics for server generation
#'
#' Calls shiny modules for the given element (block, fields).
#'
#' @param x Object for which to generate a [moduleServer()]
#' @param ... Generic consistency
#'
#' @export
block_server <- function(x, ...) {
  UseMethod("block_server")
}

#' @rdname block_server
#' @export
expr_server <- function(x, ...) {
  UseMethod("expr_server")
}

#' @rdname block_server
#' @export
expr_server.block <- function(x, ...) {
  server <- block_expr_server(x)
  server(...)
}


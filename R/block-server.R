#' Generics for server generation
#'
#' Calls shiny modules for the given element (block, fields).
#'
#' @param x Object for which to generate a [moduleServer()]
#' @param data Input data (list of reactives)
#' @param ... Generic consistency
#'
#' @export
block_server <- function(x, data, ...) {
  UseMethod("block_server")
}

#' @rdname block_server
#' @export
block_server.block <- function(x, data, ...) {
  moduleServer(
    block_uid(x),
    function(input, output, session) {

      exp <- expr_server(x, data)
      res <- reactive(eval(exp$expr(), lapply(data, reval)))

      output$result <- block_output(x, res)

      c(
        list(result = res),
        exp
      )
    }
  )
}

#' @rdname block_server
#' @export
expr_server <- function(x, data, ...) {
  UseMethod("expr_server")
}

#' @rdname block_server
#' @export
expr_server.block <- function(x, data, ...) {
  do.call(block_expr_server(x), data)
}

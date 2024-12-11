#' @rdname new_block
#' @export
new_data_block <- function(expr_server, expr_ui, class, ...) {
  new_block(expr_server, expr_ui, c(class, "data_block"), ...)
}

#' @rdname block_server
#' @export
block_server.data_block <- function(x, ...) {
  moduleServer(
    block_uid(x),
    function(input, output, session) {

      expr <- expr_server(x)

      result <- reactive(eval(expr$expr()))

      output$result <- block_output(x, result)

      c(
        list(result = result),
        expr
      )
    }
  )
}

#' @rdname block_ui
#' @export
block_ui.data_block <- function(x, ...) {
  tagList(
    expr_ui(x, ...),
    DT::dataTableOutput(block_ns(x, "result"))
  )
}

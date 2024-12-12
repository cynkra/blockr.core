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

      exp_srv <- expr_server(x)

      expr <- reactive(eval(exp_srv$expr()))
      res <- reactive(eval(expr()))

      output$result <- block_output(x, res)

      c(
        list(result = res),
        exp_srv
      )
    }
  )
}

#' @rdname block_ui
#' @export
block_ui.data_block <- function(x, id = NULL, ...) {
  tagList(
    expr_ui(x, block_ns(x, namespace = id), ...),
    DT::dataTableOutput(block_ns(x, "result", namespace = id))
  )
}

#' @rdname block_server
#' @export
expr_server.data_block <- function(x, ...) {
  server <- block_expr_server(x)
  server(...)
}

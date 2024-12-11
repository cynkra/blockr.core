#' @rdname new_block
#' @export
new_data_block <- function(expr_server, expr_ui, class, ...) {
  new_block(expr_server, expr_ui, c(class, "data_block"), ...)
}

#' @rdname block_server
#' @export
block_server.data_block <- function(x, data = list(), ...) {
  moduleServer(
    block_uid(x),
    function(input, output, session) {

      fields <- expr_server(x)

      result <- reactive(evaluate_block(x, values = lapply(fields, reval)))

      output$result <- block_output(x, result)

      list(
        result = result,
        code = reactive(generate_code(x, values = lapply(fields, reval)))
      )
    }
  )
}

#' @rdname block_ui
#' @export
block_ui.data_block <- function(x, ...) {
  tagList(
    expr_ui(x)(...),
    DT::dataTableOutput(block_ns(x, "result"))
  )
}

#' @rdname new_block
#' @export
new_transform_block <- function(expr_server, expr_ui, class, ...) {
  new_block(expr_server, expr_ui, c(class, "transform_block"), ...)
}

#' @param data Input data (list of reactives)
#' @rdname block_server
#' @export
block_server.transform_block <- function(x, data, ...) {
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

#' @rdname block_ui
#' @export
block_ui.transform_block <- function(x, id = NULL, ...) {
  tagList(
    expr_ui(x, block_ns(x, namespace = id), ...),
    DT::dataTableOutput(block_ns(x, "result", namespace = id))
  )
}

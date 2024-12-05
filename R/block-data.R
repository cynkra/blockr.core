#' @rdname block_server
#' @export
block_server.data_block <- function(x, input = list(), ...) {
  moduleServer(
    block_uid(x, prefix = "block"),
    function(input, output, session) {

      fields <- fields_server(x, input)
      result <- reactive(evaluate_block(x, values = lapply(fields, reval)))

      output$result <- DT::renderDT(
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
    fields_ui(x),
    DT::dataTableOutput(block_ns(x, "result", prefix = "block"))
  )
}

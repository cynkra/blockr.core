#' @rdname new_block
#' @export
new_data_block <- function(server, ui, class, ctor = sys.parent(), ...) {

  new_block(server, ui, c(class, "data_block"), ctor, ...)
}

#' @rdname block_ui
#' @export
block_output.data_block <- function(x, result) {
  DT::renderDT(
    DT::datatable(
      result,
      selection = "none",
      options = list(
        pageLength = 5L,
        processing = FALSE
      )
    ),
    server = TRUE
  )
}

#' @rdname block_ui
#' @export
block_ui.data_block <- function(id, x, ...) {
  tagList(
    expr_ui(id, x, ...),
    DT::dataTableOutput(NS(id, "result"))
  )
}

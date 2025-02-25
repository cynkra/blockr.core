#' @rdname new_block
#' @export
new_parser_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "parser_block"), ctor, ...)
}

#' @rdname block_ui
#' @export
block_output.parser_block <- function(x, result, session) {
  dt_result(result, session)
}

#' @rdname block_ui
#' @export
block_ui.parser_block <- function(id, x, ...) {
  tagList(
    DT::dataTableOutput(NS(id, "result"))
  )
}

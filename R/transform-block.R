#' @rdname new_block
#' @export
new_transform_block <- function(server, ui, class,
                                ctor = sys.parent(), ...) {

  new_block(server, ui, c(class, "transform_block"), ctor, ...)
}

#' @rdname block_ui
#' @export
block_output.transform_block <- function(x, result, session) {
  dt_result(result, session)
}

#' @rdname block_ui
#' @export
block_ui.transform_block <- function(id, x, ...) {
  tagList(
    DT::dataTableOutput(NS(id, "result"))
  )
}

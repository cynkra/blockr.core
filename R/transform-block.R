#' @rdname new_block
#' @export
new_transform_block <- function(server, ui, class,
                                ctor = sys.parent(), ...) {

  new_block(server, ui, c(class, "transform_block"), ctor, ...)
}

#' @rdname block_ui
#' @export
block_output.transform_block <- function(x, result) {
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
block_ui.transform_block <- function(id, x, ...) {
  tagList(
    DT::dataTableOutput(NS(id, "result"))
  )
}

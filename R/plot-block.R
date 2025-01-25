#' @rdname new_block
#' @export
new_plot_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "plot_block"), ctor, ...)
}

#' @rdname block_ui
#' @export
block_output.plot_block <- function(x, result) {
  renderPlot(grDevices::replayPlot(attr(result, "plot")))
}

#' @rdname block_ui
#' @export
block_ui.plot_block <- function(id, x, ...) {
  tagList(
    expr_ui(id, x, ...),
    plotOutput(NS(id, "result"))
  )
}

#' @rdname block_server
#' @export
block_eval.plot_block <- function(x, expr, data, ...) {
  structure(coal(NextMethod(), list()), plot = grDevices::recordPlot())
}

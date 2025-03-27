#' @rdname new_block
#' @export
new_plot_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "plot_block"), ctor, ...)
}

#' @rdname block_ui
#' @export
block_output.plot_block <- function(x, result, session) {
  plt <- attr(result, "plot")
  req(plt)
  renderPlot(grDevices::replayPlot(plt))
}

#' @rdname block_ui
#' @export
block_ui.plot_block <- function(id, x, ...) {
  tagList(
    plotOutput(NS(id, "result"))
  )
}

#' @rdname block_server
#' @export
block_eval.plot_block <- function(x, expr, data, ...) {
  structure(coal(NextMethod(), list()), plot = grDevices::recordPlot())
}

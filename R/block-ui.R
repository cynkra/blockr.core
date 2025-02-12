#' Generics for UI generation
#'
#' Generics for creating fields and block UI containers.
#'
#' @param id Namespace ID
#' @param x Object for which to generate a UI container
#' @param ... Generic consistency
#'
#' @export
block_ui <- function(id, x, ...) {
  UseMethod("block_ui", x)
}

#' @rdname block_ui
#' @export
block_ui.block <- function(id, x, ...) {
  expr_ui(id, x, ...)
}

#' @rdname block_ui
#' @export
expr_ui <- function(id, x, ...) {
  UseMethod("expr_ui", x)
}

#' @rdname block_ui
#' @export
expr_ui.block <- function(id, x, ...) {

  if (...length()) {
    abort(
      paste(
        "Unknown arguments", paste_enum(...names()), "in call to `expr_ui()`."
      ),
      class = "superfluous_expr_ui_args"
    )
  }

  do.call(block_expr_ui(x), list(id = NS(id, "expr")))
}

#' @param result Reactive block result
#' @rdname block_ui
#' @export
block_output <- function(x, result) {
  UseMethod("block_output")
}

#' @rdname block_ui
#' @export
block_output.block <- function(x, result) {
  NULL
}

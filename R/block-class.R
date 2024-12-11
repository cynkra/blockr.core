#' Blocks
#'
#' Blocks consist of an expression that defines what the block produces (given
#' the result of the previous block combined with user input), plus a class
#' attribute
#'
#' @param expr_server A function returning [shiny::moduleServer()]
#' @param expr_ui A function with a single argument (`ns`) returning a
#'   `shiny.tag`
#' @param class Block subclass
#' @param uid Unique block ID
#' @param ... Further (metadata) attributes
#'
#' @export
new_block <- function(expr_server, expr_ui, class, uid = rand_names(), ...) {

  stopifnot(
    is.function(expr_server), is.function(expr_ui),
    is.character(class), length(class) > 0L, is_string(uid)
  )

	structure(
    list(expr_server = expr_server, expr_ui = expr_ui),
    ...,
    uid = uid,
    class = c(class, "block")
  )
}

#' @param x An object inheriting from `"block"`
#' @rdname new_block
#' @export
is_block <- function(x) {
  inherits(x, "block")
}

#' @rdname new_block
#' @export
block_uid <- function(x) {
  attr(x, "uid")
}

#' @rdname new_block
#' @export
block_ns <- function(x, ...) {

  fun <- function(...) {

    ns <- NS(block_uid(x))

    len <- ...length()

    if (len > 1L) {
      for (i in seq_len(len - 1L)) {
        ns <- NS(ns(...elt(i)))
      }
    }

    if (len > 0L) {
      return(ns(...elt(len)))
    }

    ns
  }

  if (...length()) {
    return(fun(...))
  }

  fun
}

#' @rdname new_block
#' @export
block_expr_server <- function(x) {
  x[["expr_server"]]
}

#' @rdname new_block
#' @export
block_expr_ui <- function(x) {
  x[["expr_ui"]]
}

#' @param data Data input
#' @rdname serve
#' @export
serve.block <- function(x, data = list(), ...) {

  ui <- bslib::page_fluid(block_ui(x))

  server <- function(input, output, session) {
    block_server(x, data)
  }

  shinyApp(ui, server)
}

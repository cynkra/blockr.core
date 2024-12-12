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

#' @param namespace (Optional) parent namespace
#' @rdname new_block
#' @export
block_ns <- function(x, ..., namespace = NULL) {

  prefix <- paste(c(namespace, block_uid(x)), collapse = ns.sep)

  fun <- function(...) {

    if (...length() == 0L) {
      return(prefix)
    }

    id <- paste(c(...), collapse = ns.sep)

    if (length(prefix) == 0) {
      return(id)
    }

    paste(prefix, id, sep = ns.sep)
  }

  if (...length() == 0L) {
    return(fun)
  }

  fun(...)
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
serve.block <- function(x, data, ...) {

  ui <- bslib::page_fluid(block_ui(x))

  if (missing(data)) {

    server <- function(input, output, session) {
      block_server(x)
    }

  } else {

    server <- function(input, output, session) {
      block_server(x, lapply(data, reactiveVal))
    }

  }

  shinyApp(ui, server)
}

#' @rdname new_block
#' @export
block_inputs <- function(x) {
  formals(block_expr_server(x))
}

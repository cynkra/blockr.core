#' Blocks
#'
#' Blocks consist of an expression that defines what the block produces (given
#' the result of the previous block combined with user input), plus a class
#' attribute
#'
#' @param server A function returning [shiny::moduleServer()]
#' @param ui A function with a single argument (`ns`) returning a `shiny.tag`
#' @param class Block subclass
#' @param uid Unique block ID
#' @param ... Further (metadata) attributes
#'
#' @export
new_block <- function(server, ui, class, uid = rand_names(), ...) {

  stopifnot(
    is.function(server), is.function(ui),
    "ns" == names(formals(ui))[1L],
    is.character(class), length(class) > 0L, is_string(uid)
  )

	res <- structure(
    list(expr_server = server, expr_ui = ui),
    ...,
    uid = uid,
    class = c(class, "block")
  )

  stopifnot(
    inherits(expr_ui(res), "shiny.tag", "shiny.tag.list", agg = any)
  )

  res
}

#' @param x An object inheriting from `"block"`
#' @rdname new_block
#' @export
is_block <- function(x) {
  inherits(x, "block")
}

#' @rdname new_block
#' @export
new_data_block <- function(server, ui, class, ...) {
  new_block(server, ui, c(class, "data_block"), ...)
}

#' @rdname new_block
#' @export
new_transform_block <- function(server, ui, class, ...) {
  new_block(server, ui, c(class, "transform_block"), ...)
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

  server <- function(input, output, session) {
    block_server(x, lapply(data, reactiveVal))
  }

  shinyApp(ui, server)
}

#' @rdname serve
#' @export
serve.data_block <- function(x, ...) {
  NextMethod(data = list())
}

#' @rdname new_block
#' @export
block_inputs <- function(x) {
  names(formals(block_expr_server(x)))
}

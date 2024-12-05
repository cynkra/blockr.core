#' Blocks
#'
#' Blocks consist of an expression that defines what the block produces (given
#' the result of the previous block combined with user input), plus a class
#' attribute
#'
#' @param expr A quoted expression (compatible with partial substitution as
#' implemented in [base::bquote()] and intended for evaluation in the context
#' of the fields)
#' @param class Block subclass
#' @param state List of values defining the block state
#' @param splice See [base::bquote()]
#' @param uid Unique block ID
#' @param ... Further (metadata) attributes
#'
#' @export
new_block <- function(expr, class, state = list(), splice = FALSE,
                      uid = rand_names(), ...) {

  stopifnot(is.language(expr), is.character(class), length(class) > 0L,
            is.list(state))

	structure(expr, state = state, splice = splice, ..., uid = uid,
            class = c(class, "block"))
}

#' @param x An object inheriting from `"block"`
#' @rdname new_block
#' @export
is_block <- function(x) {
  inherits(x, "block")
}

#' @param prefix UID prefix (for module ID generation)
#' @rdname new_block
#' @export
block_uid <- function(x, prefix = character()) {

  res <- attr(x, "uid")

  if (length(prefix)) {
    stopifnot(is_string(prefix))
    return(paste0(prefix, "_", res))
  }

  res
}

#' @param input_id Input ID (for ns generation)
#' @rdname new_block
#' @export
block_ns <- function(x, input_id = character(), prefix = character()) {

  ns <- NS(block_uid(x, prefix))

  if (length(input_id)) {
    return(ns(input_id))
  }

  ns
}

#' @param selection State attribute to return
#' @rdname new_block
#' @export
block_state <- function(x, selection = character()) {

  res <- attr(x, "state")

  if (length(selection)) {
    return(res[[selection]])
  }

  res
}

#' @rdname new_block
#' @export
block_expr <- function(x) {
  attributes(x) <- NULL
  x
}

#' @rdname new_block
#' @export
block_splice <- function(x) {
  attr(x, "splice")
}

#' Interpolate block expression
#'
#' For a given block, generate the code using the block expression by
#' interpolating using [base::bquote()].
#'
#' @param x An object inheriting from `block`
#' @param input Data input
#' @param values Block field values
#'
#' @export
interpolate_expr <- function(x, input = list(), values = block_state(x)) {
  UseMethod("interpolate_expr")
}

#' @rdname interpolate_expr
#' @export
interpolate_expr.block <- function(x, input = list(), values = block_state(x)) {
  args <- c(lapply(input, as.name), values)
  do.call(bquote, list(block_expr(x), where = args, splice = block_splice(x)))
}

#' @param ... Forwarded to [interpolate_expr()]
#' @rdname interpolate_expr
#' @export
generate_code <- function(x, ...) {
  UseMethod("generate_code")
}

#' @rdname interpolate_expr
#' @export
generate_code.block <- function(x, ...) {
  bquote(
    .(lhs) <- .(rhs),
    list(lhs = as.name(block_uid(x)), rhs = interpolate_expr(x, ...))
  )
}

#' @param env Data envrionment
#' @rdname interpolate_expr
#' @export
evaluate_block <- function(x, env = list(), ...) {
  UseMethod("evaluate_block")
}

#' @rdname interpolate_expr
#' @export
evaluate_block.block <- function(x, env = list(), ...) {
  eval(interpolate_expr(x, ...), env)
}

#' @param input Data input
#' @rdname serve
#' @export
serve.block <- function(x, input = list(), ...) {

  ui <- bslib::page_fluid(block_ui(x))

  data_input <- input

  server <- function(input, output, session) {
    block_server(x, data_input)
  }

  shinyApp(ui, server)
}

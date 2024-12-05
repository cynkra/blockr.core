#' Blocks
#'
#' Blocks consist of an expression that defines what the block produces (given
#' the result of the previous block combined with user input), plus a class
#' attribute
#'
#' @param expr A quoted expression (compatible with partial substitution as
#' implemented in [base::bquote()] and intended for evaluation in the context
#' of the fields)
#' @param ... Further (metadata) attributes
#' @param class Block subclass
#'
#' @export
new_block <- function(expr, class, ...) {

  stopifnot(is.language(expr), is_string(class))

	structure(expr, ..., class = c(class, "block"))
}

#' @param x An object inheriting from `"block"`
#' @rdname new_block
#' @export
is_block <- function(x) {
  inherits(x, "block")
}

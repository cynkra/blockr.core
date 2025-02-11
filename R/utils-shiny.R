reorder_rv <- function(x, new) {

  stopifnot(
    is.reactivevalues(x), setequal(new, names(x)), anyDuplicated(new) == 0L
  )

  internals <- .subset2(x, "impl")
  internals$.nameOrder <- new

  invisible(x)
}

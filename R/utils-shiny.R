reorder_rv <- function(x, new) {

  stopifnot(
    is.reactivevalues(x), setequal(new, names(x)), anyDuplicated(new) == 0L
  )

  internals <- .subset2(x, "impl")
  internals$.nameOrder <- new

  invisible(x)
}

make_read_only <- function(x) {

  stopifnot(is.reactivevalues(x))

  res <- unclass(x)
  res[["readonly"]] <- TRUE
  class(res) <- class(x)

  res
}

#' @importFrom generics intersect
#' @export
intersect.character <- function(x, y, ...) {
  if (!is_stack(y)) {
    NextMethod()
  } else {
    vec_set_intersect(x, y, ...)
  }
}

#' @importFrom generics union
#' @export
union.character <- function(x, y, ...) {
  if (!is_stack(y)) {
    NextMethod()
  } else {
    vec_set_union(x, y, ...)
  }
}

#' @importFrom generics setdiff
#' @export
setdiff.character <- function(x, y, ...) {
  if (!is_stack(y)) {
    NextMethod()
  } else {
    vec_set_difference(x, y, ...)
  }
}

#' Set operations
#'
#' Union (`union()`), intersect (`intersect()`), difference (`setdiff()`),
#' and equality (`setequal()`) for two vectors representing sets.
#'
#' These functions override the set functions provided in base to make them
#' generic so that packages can provide methods for different data types. The
#' default methods call the base versions.
#'
#' @param x,y Vectors to combine.
#' @param ... Other arguments passed on to methods.
#' @return A vector with all duplicates removed.
#'
#' @name setops
#' @examples
#' intersect(1:5, 4:8)
#' union(1:5, 4:8)
#'
#' setdiff(1:5, 4:8)
#' setdiff(4:8, 1:5)
NULL

#' @rdname setops
#' @export
intersect <- function(x, y, ...) {
  UseMethod("intersect", vec_ptype2(x, y))
}

#' @rdname setops
#' @export
union <- function(x, y, ...) {
  UseMethod("union", vec_ptype2(x, y))
}

#' @rdname setops
#' @export
setdiff <- function(x, y, ...) {
  UseMethod("setdiff", vec_ptype2(x, y))
}

#' @export
intersect.default <- function(x, y, ...) {
  base::intersect(x, y, ...)
}

#' @export
union.default <- function(x, y, ...) {
  base::union(x, y, ...)
}

#' @export
setdiff.default <- function(x, y, ...) {
  base::setdiff(x, y, ...)
}

chr_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, character(length), ..., USE.NAMES = use_names)
}

#' @keywords internal
lgl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, logical(length), ..., USE.NAMES = use_names)
}

int_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, integer(length), ..., USE.NAMES = use_names)
}

dbl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, double(length), ..., USE.NAMES = use_names)
}

chr_mply <- function(...) {
  chr_ply(map(...), identity)
}

lgl_mply <- function(...) {
  lgl_ply(map(...), identity)
}

int_mply <- function(...) {
  int_ply(map(...), identity)
}

dbl_mply <- function(...) {
  dbl_ply(map(...), identity)
}

chr_xtr <- function(x, i, ...) chr_ply(x, `[[`, i, ...)

lgl_xtr <- function(x, i, ...) lgl_ply(x, `[[`, i, ...)

int_xtr <- function(x, i, ...) int_ply(x, `[[`, i, ...)

dbl_xtr <- function(x, i, ...) dbl_ply(x, `[[`, i, ...)

lst_xtr <- function(x, ...) {

  for (i in c(...)) {
    x <- lapply(x, `[[`, i)
  }

  x
}

map <- function(f, ..., use_names = FALSE) Map(f, ..., USE.NAMES = use_names)

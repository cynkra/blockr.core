#' @import vctrs
#' @keywords internal
"_PACKAGE"

#' @importFrom rlang abort warn inform
#' @import shiny
NULL

pkg_name <- function(env = parent.frame()) {
  utils::packageName(env)
}

pkg_version <- function(pkg = pkg_name()) {
  utils::packageVersion(pkg)
}

pkg_file <- function(...) {
  system.file(..., package = pkg_name())
}

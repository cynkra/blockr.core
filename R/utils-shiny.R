#' Custom shiny textInput
#'
#' A shiny textInput where one can set the debounce rate.
#' Defaults to 2000ms.
#'
#' @param ... Forwarded to [shiny::textInput()]
#' @param debounce Debounce delay in ms.
#' @keywords internal
slow_text_input <- function(..., debounce = 1000) {

  stopifnot(is_count(debounce))

  res <- htmltools::tagQuery(
    textInput(...)
  )$find(
    ".shiny-input-text"
  )$removeClass(
    "shiny-input-text"
  )$addClass(
    "shiny-slow-text"
  )$addAttrs(
    "data-debounce" = debounce
  )$allTags()

  tagList(res, slow_text_input_binding())
}

#' Custom binding slow text input
#'
#' @keywords internal
slow_text_input_binding <- function() {
  htmltools::htmlDependency(
    "slow-text-binding",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "slowTextInputBinding.js"
  )
}

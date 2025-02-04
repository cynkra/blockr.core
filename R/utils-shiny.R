#' Custom shiny textInput
#'
#' A shiny textInput where one can set the debounce rate.
#' Defaults to 2000ms.
#'
#' @param ... Forwarded to [shiny::textInput()]
#' @param debounce Debounce delay in ms.
#' @keywords internal
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bslib)
#'   ui <- page_fluid(
#'     blockr.core:::slow_text_input("ee", "ee", "aaa", debounce = 5000)
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$ee, {
#'       showNotification(input$ee)
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
slow_text_input <- function(..., debounce = 1000) {
  stopifnot(is_count(debounce))

  tagList(
    htmltools::tagQuery(
      textInput(...)
    )$find(".shiny-input-text")$removeClass("shiny-input-text")$addClass(
      "shiny-slow-text"
    )$addAttrs("data-debounce" = debounce)$allTags(),
    slow_text_input_binding()
  )
}

#' Custom binding slow text input
#'
#' @keywords internal
slow_text_input_binding <- function() {
  htmltools::htmlDependency(
    "slow-text-binding",
    utils::packageVersion(utils::packageName()),
    src = system.file("assets/js", package = "blockr.core"),
    script = "slowTextInputBinding.js"
  )
}

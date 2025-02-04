#' Custom shiny textInput
#'
#' A shiny textInput where one can set the debounce rate.
#' Defaults to 2000ms.
#'
#' @inheritParams shiny::textInput
#' @param debounce Debounce delay in ms.
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(bslib)
#'
#'  ui <- page_fluid(
#'    slow_text_input("ee", "ee", "aaa")
#'  )
#'
#'  server <- function(input, output, session) {
#'    observeEvent(input$ee, {
#'      showNotification(input$ee)
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#' }
slow_text_input <- function(
  inputId,
  label,
  value = "",
  width = NULL,
  placeholder = NULL,
  debounce = 2000
) {
  stopifnot(is.numeric(debounce), debounce > 0)

  binding <- htmltools::htmlDependency(
    "slow-text-binding",
    utils::packageVersion(utils::packageName()),
    src = system.file("assets/js", package = "blockr.core"),
    script = "slowTextInputBinding.js"
  )

  tagList(
    htmltools::tagQuery(
      shiny::textInput(inputId, label, value, width, placeholder)
    )$find(".shiny-input-text")$removeClass("shiny-input-text")$addClass(
      "shiny-slow-text"
    )$addAttrs("data-debounce" = debounce)$allTags(),
    binding
  )
}

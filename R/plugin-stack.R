#' Edit stack module
#'
#' Customizable logic for editing stack attributes such as stack title.
#'
#' @param id Namespace ID
#' @param stack Stack (reactive)
#' @param board Reactive values object containing board information
#' @param update Reactive value object to initiate board updates
#' @param ... Extra arguments passed from parent scope
#'
#' @return A list of [shiny::reactiveVal()] objects interpreted as block state
#' components.
#'
#' @rdname edit_stack
#' @export
edit_stack_server <- function(id, stack, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      output$stack_name_out <- renderUI(
        bslib::tooltip(stack_name(stack()), paste("Stack ID: ", id))
      )

      observeEvent(
        input$stack_name_in,
        {
          if (!identical(stack_name(stack()), input$stack_name_in)) {
            new_val <- stack()
            stack_name(new_val) <- input$stack_name_in
            stack(new_val)
          }
        },
        ignoreInit = TRUE
      )

      NULL
    }
  )
}

#' @param x Stack
#' @rdname edit_stack
#' @export
edit_stack_ui <- function(id, x, ...) {
  browser()
  tagList(
    uiOutput(NS(id, "stack_name_out"), inline = TRUE),
    span(
      `data-bs-toggle` = "collapse",
      `data-bs-target` = "",
      bslib::popover(
        bsicons::bs_icon("gear"),
        textInput(
          NS(id, "stack_name_in"),
          "Stack name"
        )
      )
    )
  )
}

#' Edit stack module
#'
#' Customizable logic for editing stack attributes such as stack title.
#'
#' @param id Namespace/stack ID
#' @param stack (initial) stack object
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
        bslib::tooltip(stack_name(stack), paste("Stack ID: ", id))
      )

      NULL
    }
  )
}

#' @param x Stack
#' @rdname edit_stack
#' @export
edit_stack_ui <- function(id, x, ...) {
  tagList(
    uiOutput(NS(id, "stack_name_out"), inline = TRUE)
  )
}

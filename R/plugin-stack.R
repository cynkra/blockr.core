#' Edit stack module
#'
#' Customizable logic for editing stack attributes such as stack title.
#'
#' @param id Namespace ID
#' @param stack_id Stack ID
#' @param board Reactive values object containing board information
#' @param update Reactive value object to initiate board updates
#' @param ... Extra arguments passed from parent scope
#'
#' @return `NULL`
#'
#' @rdname edit_stack
#' @export
edit_stack_server <- function(id, stack_id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      cur_name <- reactive(
        stack_name(board_stacks(board$board)[[stack_id]])
      )

      output$stack_name_out <- renderUI(
        bslib::tooltip(cur_name(), paste("Stack ID: ", stack_id))
      )

      observeEvent(
        cur_name(),
        updateTextInput(
          session,
          "stack_name_in",
          "Stack name",
          cur_name()
        )
      )

      observeEvent(
        input$stack_name_in,
        {
          req(input$stack_name_in)
          if (!identical(cur_name(), input$stack_name_in)) {
            new_val <- board_stacks(board$board)[[stack_id]]
            stack_name(new_val) <- input$stack_name_in
            new_val <- as_stacks(set_names(list(new_val), stack_id))
            update(list(stacks = list(mod = new_val)))
          }
        }
      )

      observeEvent(
        input$rm_stack,
        update(list(stacks = list(rm = stack_id)))
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
    uiOutput(NS(id, "stack_name_out"), inline = TRUE),
    span(
      `data-bs-toggle` = "collapse",
      `data-bs-target` = "",
      bslib::popover(
        bsicons::bs_icon("gear"),
        textInput(
          NS(id, "stack_name_in"),
          "Stack name",
          stack_name(x)
        ),
        actionButton(
          NS(id, "rm_stack"),
          "Remove stack",
          icon = icon("circle-minus"),
          class = c("btn-danger", "mb-2")
        )
      )
    )
  )
}

#' Add/remove block module
#'
#' Customizable logic for adding/removing blocks to the board.
#'
#' @param id Namespace ID
#' @param rv Reactive values object
#' @param update Reactive value object to initiate board updates
#' @param ... Extra arguments passed from parent scope
#'
#' @return A [shiny::reactiveValues()] object with components `add` and `rm`,
#' where `add` may be `NULL` or a `block` object and `rm` be `NULL` or a string
#' (block ID).
#'
#' @rdname add_rm_block
#' @export
add_rm_block_server <- function(id, rv, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(
        input$add_block,
        showModal(add_block_modal(session$ns))
      )

      observeEvent(
        input$confirm_add,
        {
          sel <- input$registry_select
          bid <- input$block_id

          if (nchar(bid) == 0L || !is_string(bid)) {

            showNotification(
              "Please choose a valid block ID.",
              type = "warning"
            )

            return()
          }

          if (bid %in% board_block_ids(rv$board)) {

            showNotification(
              "Please choose a unique block ID.",
              type = "warning"
            )

            return()
          }

          if (!is_string(sel) || !sel %in% list_blocks()) {

            showNotification(
              "Please choose a valid block type.",
              type = "warning"
            )

            return()
          }

          add <- as_blocks(
            set_names(list(create_block(sel)), bid)
          )

          update(
            list(blocks = list(add = add))
          )

          removeModal()
        }
      )

      observeEvent(
        input$cancel_add,
        removeModal()
      )

      observeEvent(
        input$rm_block,
        rm_block_modal(session$ns, rv$board)
      )

      observeEvent(
        input$confirm_rm,
        {
          sel <- input$block_select

          if (!length(sel)) {

            showNotification(
              "Please choose at least one block.",
              type = "warning"
            )

            return()
          }

          if (!all(sel %in% board_block_ids(rv$board))) {

            showNotification(
              "Please choose valid block IDs.",
              type = "warning"
            )

            return()
          }

          update(
            list(blocks = list(rm = sel))
          )

          removeModal()
        }
      )

      observeEvent(
        input$cancel_rm,
        removeModal()
      )

      NULL
    }
  )
}

#' @param board The initial `board` object
#' @rdname add_rm_block
#' @export
add_rm_block_ui <- function(id, board) {
  tagList(
    actionButton(
      NS(id, "add_block"),
      "Add block",
      icon = icon("circle-plus"),
      class = "btn-success"
    ),
    actionButton(
      NS(id, "rm_block"),
      "Remove block",
      icon = icon("circle-minus"),
      class = "btn-danger"
    )
  )
}

add_block_modal <- function(ns) {
  modalDialog(
    title = "Add block",
    div(
      selectInput(
        ns("registry_select"),
        "Select block from registry",
        choices = list_blocks()
      ),
      textInput(
        inputId = ns("block_id"),
        label = "Block ID",
        value = rand_names(),
        placeholder = "Enter a block ID."
      )
    ),
    footer = tagList(
      actionButton(ns("cancel_add"), "Cancel", class = "btn-danger"),
      actionButton(ns("confirm_add"), "OK", class = "btn-success")
    )
  )
}

rm_block_modal <- function(ns, board) {
  showModal(
    modalDialog(
      title = "Remove blocks",
      selectInput(
        ns("block_select"),
        "Select block(s) from board",
        choices = board_block_ids(board),
        multiple = TRUE
      ),
      footer = tagList(
        actionButton(ns("cancel_rm"), "Cancel", class = "btn-danger"),
        actionButton(ns("confirm_rm"), "OK", class = "btn-success")
      )
    )
  )
}

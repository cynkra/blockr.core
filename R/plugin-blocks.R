#' Add/remove block module
#'
#' Customizable logic for adding/removing blocks to the board.
#'
#' @param id Namespace ID
#' @param rv Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @return A [shiny::reactiveValues()] object with components `add` and `rm`,
#' where `add` may be `NULL` or a `block` object and `rm` be `NULL` or a string
#' (block ID).
#'
#' @rdname add_rm_block
#' @export
add_rm_block_server <- function(id, rv, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      res <- reactiveValues(add = NULL, rm = NULL)

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

          res$add <- as_blocks(
            set_names(list(create_block(sel)), bid)
          )

          removeModal()
        }
      )

      observeEvent(
        input$cancel_add,
        {
          res$add <- NULL
          removeModal()
        }
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

          res$rm <- sel
          removeModal()
        }
      )

      observeEvent(
        input$cancel_rm,
        {
          res$rm <- NULL
          removeModal()
        }
      )

      res
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

check_add_rm_block_val <- function(val, rv) {
  observeEvent(
    TRUE,
    {
      if (!is.reactivevalues(val)) {
        abort(
          "Expecting `manage_blocks` to return a `reactivevalues` object.",
          class = "manage_blocks_return_invalid"
        )
      }

      if (!setequal(names(val), c("add", "rm"))) {
        abort(
          paste(
            "Expecting the `manage_blocks` return value to contain",
            "components `add` and `rm`."
          ),
          class = "manage_blocks_return_invalid"
        )
      }
    },
    once = TRUE
  )

  observeEvent(
    val$add,
    {
      if (!is_blocks(val$add)) {
        abort(
          paste(
            "Expecting the `add` component of the `manage_blocks` return",
            "value to be `NULL` or a `blocks` object."
          ),
          class = "manage_blocks_return_invalid"
        )
      }
    },
    once = TRUE,
    priority = 2
  )

  observeEvent(
    val$add,
    {
      if (any(names(val$add) %in% board_block_ids(rv$board))) {
        abort(
          "Expecting the newly added block to have a unique ID.",
          class = "manage_blocks_return_invalid"
        )
      }
    },
    priority = 1
  )

  observeEvent(
    val$rm,
    {
      if (!is.character(val$rm)) {
        abort(
          paste(
            "Expecting the `rm` component of the `manage_blocks` return",
            "value to be `NULL` or a character vector."
          ),
          class = "manage_blocks_return_invalid"
        )
      }
    },
    once = TRUE,
    priority = 2
  )

  observeEvent(
    val$rm,
    {
      if (all(!val$rm %in% board_block_ids(rv$board))) {
        abort(
          "Expecting the removed block to be specified by a known ID.",
          class = "manage_blocks_return_invalid"
        )
      }
    },
    priority = 1
  )

  val
}

#' Add/remove block module
#'
#' Customizable logic for adding/removing blocks to the board.
#'
#' @param rv Reactive values object
#'
#' @return A [shiny::reactiveValues()] object with components `add` and `rm`,
#' where `add` may be `NULL` or a `block` object and `rm` be `NULL` or a string
#' (block ID).
#'
#' @rdname add_rm_block
#' @export
add_rm_block_server <- function(rv) {
  moduleServer(
    "add_rm_block",
    function(input, output, session) {

      res <- reactiveValues(add = NULL, rm = NULL)

      observeEvent(input$add_block, {

        req(input$registry_select)
        res$add <- create_block(input$registry_select)
      })

      observe(
        updateSelectInput(
          session,
          inputId = "block_select",
          choices = block_ids(rv$board)
        )
      )

      observeEvent(input$rm_block, {

        req(input$block_select)

        res$rm <- input$block_select
      })

      res
    }
  )
}

#' @param rv Namespace ID
#' @param board The initial `board` object
#' @rdname add_rm_block
#' @export
add_rm_block_ui <- function(id, board) {

  ns <- NS(
    NS(id, "add_rm_block")
  )

  list(
    selectInput(
      ns("registry_select"),
      "Select block from registry",
      choices = c("", list_blocks())
    ),
    actionButton(
      ns("add_block"),
      "Add block",
      icon = icon("plus"),
      class = "btn-success"
    ),
    selectInput(
      ns("block_select"),
      "Select block from board",
      choices = c("", block_ids(board))
    ),
    actionButton(
      ns("rm_block"),
      "Remove block",
      icon = icon("minus"),
      class = "btn-danger"
    )
  )
}

check_add_rm_block_val <- function(val, rv) {

  observeEvent(
    TRUE,
    {
      if (!is.reactivevalues(val)) {
        stop("Expecting `add_rm_block` to return a `reactivevalues` object.")
      }

      if (!setequal(names(val), c("add", "rm"))) {
        stop("Expecting the `add_rm_block` return value to contain ",
             "components `add` and `rm`.")
      }
    },
    once = TRUE
  )

  observeEvent(
    val$add,
    {
      if (!is_block(val$add)) {
        stop("Expecting the `add` component of the `add_rm_block` return ",
             "value to be `NULL` or a `block` object.")
      }
    },
    once = TRUE
  )

  observeEvent(
    val$add,
    {
      if (block_uid(val$add) %in% block_ids(rv$board)) {
        stop("Expecting the newly added block to have a unique ID.")
      }
    },
    priority = 1
  )

  observeEvent(
    val$rm,
    {
      if (!is_string(val$rm)) {
        stop("Expecting the `rm` component of the `add_rm_block` return ",
             "value to be `NULL` or a string.")
      }
    },
    once = TRUE
  )

  observeEvent(
    val$rm,
    {
      if (!val$rm %in% block_ids(rv$board)) {
        stop("Expecting the removed block to be specified by a known ID.")
      }
    },
    priority = 1
  )

  val
}

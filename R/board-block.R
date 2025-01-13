#' Add/remove block module
#'
#' Customizable logic for adding/removing blocks to the board.
#'
#' @param rv Reactive values object
#'
#' @rdname add_rm_block
#' @export
add_rm_block_server <- function(rv) {
  moduleServer(
    "add_rm_block",
    function(input, output, session) {

      res <- reactiveValues(add = list(), rm = character())

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

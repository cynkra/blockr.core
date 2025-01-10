#' Board UI
#'
#' Shiny UI function for `board` objects.
#'
#' @param x Board
#'
#' @export
board_ui <- function(x) {
  UseMethod("board_ui")
}

#' @rdname board_ui
#' @export
board_ui.board <- function(x, new_block_ui) {

  id <- attr(x, "id")
  ns <- NS(id)

  tagList(
    div(
      class = "d-flex justify-content-center",
      downloadButton(
        ns("serialize"),
        "Save",
        class = "mx-2"
      ),
      fileInput(
        ns("restore"),
        "Restore"
      ),
      selectInput(
        ns("block_select"),
        "Select block",
        choices = list_blocks(),
        selected = ""
      ),
      actionButton(
        ns("add_block"),
        "Add block",
        icon = icon("plus"),
        class = "btn-success"
      )
    ),
    do.call(div, list(id = paste0(id, "_blocks"), block_cards(x)))
  )
}

block_cards <- function(x) {
  lapply(sort(x), block_card, id = attr(x, "id"))
}

block_card <- function(x, id) {
  div(
    class = "card shadow-sm p-2 mb-2 border",
    div(
      class = "card-body p-1",
      block_ui(x, id)
    )
  )
}

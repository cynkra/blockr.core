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

  id <- board_id(x)
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
        choices = c("", block_ids(x))
      ),
      actionButton(
        ns("rm_block"),
        "Remove block",
        icon = icon("minus"),
        class = "btn-danger"
      ),
      actionButton(
        ns("links"),
        "Edit connections",
        icon = icon("table")
      )
    ),
    do.call(div, list(id = paste0(id, "_blocks"), block_cards(x)))
  )
}

block_cards <- function(x) {
  lapply(sort(x), block_card, id = board_id(x))
}

block_card <- function(x, id) {

  stopifnot(is_block(x), is_string(id))

  div(
    class = "card shadow-sm p-2 mb-2 border",
    id = paste0(block_uid(x), "_block"),
    div(
      class = "card-body p-1",
      block_ui(x, id)
    )
  )
}

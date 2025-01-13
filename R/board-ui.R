#' Board UI
#'
#' Shiny UI function for `board` objects.
#'
#' @param x Board
#' @param ... Generic consistency
#'
#' @export
board_ui <- function(x, ...) {
  UseMethod("board_ui")
}

#' @param ser_deser UI for serialization/deserialization
#' @param add_rm_block UI for block addition/removal
#' @rdname board_ui
#' @export
board_ui.board <- function(x,
                           ser_deser = NULL,
                           add_rm_block = NULL,
                           ...) {

  id <- board_id(x)
  ns <- NS(id)

  toolbar_args <- list(
    actionButton(
      ns("links"),
      "Edit connections",
      icon = icon("table")
    )
  )

  if (not_null(ser_deser)) {
    toolbar_args <- c(
      ser_deser(id, x),
      add_rm_block(id, x),
      toolbar_args
    )
  }

  tagList(
    do.call(div, c(class = "d-flex justify-content-center", toolbar_args)),
    do.call(div, c(id = paste0(id, "_blocks"), block_cards(x)))
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

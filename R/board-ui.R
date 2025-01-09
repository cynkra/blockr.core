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
board_ui.board <- function(x) {

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
      )
    ),
    do.call(div, list(id = paste0(id, "_blocks"), block_cards(x, id)))
  )
}

block_cards <- function(x, id = attr(x, "id")) {
  lapply(
    lapply(
      lapply(sort(x), block_ui, id = id),
      div,
      class = "card-body p-1"
    ),
    div,
    class = "card shadow-sm p-2 mb-2 border"
  )
}

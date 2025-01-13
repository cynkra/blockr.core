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
#' @param add_rm_conn UI for block connection addition/removal
#' @rdname board_ui
#' @export
board_ui.board <- function(x,
                           ser_deser = NULL,
                           add_rm_block = NULL,
                           add_rm_conn = NULL,
                           ...) {

  id <- board_id(x)

  toolbar_args <- c(
    if (not_null(ser_deser)) ser_deser(id, x),
    if (not_null(add_rm_block)) add_rm_block(id, x),
    if (not_null(add_rm_conn)) add_rm_conn(id, x)
  )

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

  blk_id <- block_uid(x)

  div(
    class = "card shadow-sm p-2 mb-2 border",
    id = paste0(blk_id, "_block"),
    div(
      class = "card-body p-1",
      h5(
        class="card-title",
        paste0(block_name(x), " (", blk_id, ")")
      ),
      block_ui(x, id)
    )
  )
}

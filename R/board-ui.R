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
    do.call(div, c(id = paste0(id, "_blocks"), block_ui(x, id)))
  )
}

#' @param block (Optional) block (or ID) for which to generate the UI
#' @rdname block_ui
#' @export
block_ui.board <- function(x, id = NULL, block = NULL, ...) {

  block_card <- function(x, id) {

    blk_id <- block_uid(x)

    div(
      class = "card shadow-sm p-2 mb-2 border",
      id = paste0(blk_id, "_block"),
      div(
        class = "card-body p-1",
        h5(
          class = "card-title",
          paste0(block_name(x), " (", blk_id, ")")
        ),
        block_ui(x, id)
      )
    )
  }

  if (is.null(id)) {
    id <- board_id(x)
  }

  stopifnot(is_string(id))

  if (is.null(block)) {
    return(
      lapply(sort(x), block_card, id = id)
    )
  }

  block_card(resolve_block(block, x), id)
}

block_id_to_block <- function(id, board) {

  hit <- match(id, board_block_ids(board))

  if (is.na(hit)) {
    stop("Unknown block ", id)
  }

  board_blocks(board)[[hit]]
}

resolve_block <- function(block, board) {

  if (is_string(block)) {
    block <- block_id_to_block(block, board)
  }

  stopifnot(is_block(block))

  block
}

#' @param block (Optional) block (or ID) for which to insert/remove the UI
#' @rdname board_ui
#' @export
insert_block_ui <- function(x, block = NULL, ...) {
  UseMethod("insert_block_ui")
}

#' @rdname board_ui
#' @export
insert_block_ui.board <- function(x, block = NULL, ...) {

  if (is.null(block)) {
    insertUI(
      paste0("#", board_id(x), "_blocks"),
      "afterBegin",
      block_ui(x)
    )
  } else {
    insertUI(
      paste0("#", board_id(x), "_blocks"),
      "beforeEnd",
      block_ui(x, block = resolve_block(block, x))
    )

  }
}

resolve_id <- function(block, board) {

  if (is_block(block)) {
    block <- block_uid(block)
  }

  stopifnot(is_string(block), block %in% board_block_ids(board))

  block
}

#' @rdname board_ui
#' @export
remove_block_ui <- function(x, block = NULL, ...) {
  UseMethod("remove_block_ui")
}

#' @rdname board_ui
#' @export
remove_block_ui.board <- function(x, block = NULL, ...) {

  if (is.null(block)) {
    removeUI(
      paste0("#", board_id(x), "_blocks > div")
    )
  } else {
    removeUI(
      paste0("#", resolve_id(block, x), "_block")
    )
  }
}

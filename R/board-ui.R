#' Board UI
#'
#' Shiny UI function for `board` objects.
#'
#' @param id Namespace ID
#' @param x Board
#' @param ... Generic consistency
#'
#' @export
board_ui <- function(id, x, ...) {
  UseMethod("board_ui", x)
}

#' @param ser_deser UI for serialization/deserialization
#' @param add_rm_block UI for block addition/removal
#' @param add_rm_link UI for block link addition/removal
#' @param block_notifications UI for block notifications
#' @rdname board_ui
#' @export
board_ui.board <- function(id,
                           x,
                           ser_deser = NULL,
                           add_rm_block = NULL,
                           add_rm_link = NULL,
                           block_notifications = NULL,
                           ...) {

  ns <- NS(id)

  toolbar_args <- list(
    if (length(ser_deser)) ser_deser(ns("ser_deser"), x),
    if (length(add_rm_block)) add_rm_block(ns("add_rm_block"), x),
    if (length(add_rm_link)) add_rm_link(ns("add_rm_link"), x)
  )

  toolbar_args <- do.call(tagList, toolbar_args)

  if (length(block_notifications)) {
    block_notifications <- block_notifications(ns("block_notifications"), x)
  } else {
    block_notifications <- tagList()
  }

  tagList(
    do.call(div, c(class = "d-flex justify-content-center", toolbar_args)),
    do.call(div, block_notifications),
    do.call(div, c(id = paste0(id, "_blocks"), block_ui(id, x)))
  )
}

#' @param blocks (Additional) blocks (or IDs) for which to generate the UI
#' @rdname block_ui
#' @export
block_ui.board <- function(id, x, blocks = NULL, ...) {

  block_card <- function(x, id, ns) {

    div(
      class = "card shadow-sm p-2 mb-2 border",
      id = paste0(id, "_block"),
      div(
        class = "card-body p-1",
        h5(
          class = "card-title",
          paste0(block_name(x), " (", id, ")")
        ),
        block_ui(ns(id), x)
      )
    )
  }

  stopifnot(is_string(id))

  if (is.null(blocks)) {
    blocks <- sort(x)
  } else if (is.character(blocks)) {
    blocks <- board_blocks(x)[blocks]
  }

  stopifnot(is_blocks(blocks))

  tagList(
    map(block_card, blocks, names(blocks), MoreArgs = list(ns = NS(id)))
  )
}

#' @param blocks (Additional) blocks (or IDs) for which to generate the UI
#' @rdname board_ui
#' @export
insert_block_ui <- function(id, x, blocks = NULL, ...) {
  UseMethod("insert_block_ui", x)
}

#' @rdname board_ui
#' @export
insert_block_ui.board <- function(id, x, blocks = NULL, ...) {

  stopifnot(is_string(id))

  insertUI(
    paste0("#", id, "_blocks"),
    "beforeEnd",
    block_ui(id, x, blocks),
    immediate = TRUE
  )
}

#' @rdname board_ui
#' @export
remove_block_ui <- function(id, x, blocks = NULL, ...) {
  UseMethod("remove_block_ui", x)
}

#' @rdname board_ui
#' @export
remove_block_ui.board <- function(id, x, blocks = NULL, ...) {

  if (is.null(blocks)) {

    stopifnot(is_string(id))

    removeUI(
      paste0("#", id, "_blocks > div"),
      multiple = TRUE,
      immediate = TRUE
    )

  } else {

    stopifnot(is.character(blocks), all(blocks %in% board_block_ids(x)))

    for (block in blocks) {
      removeUI(
        paste0("#", block, "_block"),
        immediate = TRUE
      )
    }
  }
}

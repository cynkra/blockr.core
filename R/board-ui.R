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

#' @param plugins UI for board plugins
#' @rdname board_ui
#' @export
board_ui.board <- function(id, x, plugins = list(), ...) {
  validate_plugins(plugins)

  ser_deser <- get_plugin("preserve_board", plugins)
  add_rm_block <- get_plugin("manage_blocks", plugins)
  add_rm_link <- get_plugin("manage_links", plugins)
  add_rm_stack <- get_plugin("manage_stacks", plugins)
  block_notifications <- get_plugin("notify_user", plugins)
  gen_code <- get_plugin("generate_code", plugins)

  ns <- NS(id)

  toolbar_args <- list(
    if (length(ser_deser)) ser_deser(ns("preserve_board"), x),
    if (length(add_rm_block)) add_rm_block(ns("manage_blocks"), x),
    if (length(add_rm_link)) add_rm_link(ns("manage_links"), x),
    if (length(add_rm_stack)) add_rm_stack(ns("manage_stacks"), x),
    if (length(gen_code)) gen_code(ns("generate_code"), x)
  )

  toolbar_args <- do.call(tagList, toolbar_args)

  if (length(block_notifications)) {
    block_notifications <- block_notifications(ns("notify_user"), x)
  } else {
    block_notifications <- tagList()
  }

  tagList(
    do.call(
      div,
      c(
        class = paste(
          "d-flex justify-content-evenly align-items-center",
          "bg-light-subtle sticky-top border rounded-4",
          "m-2 gap-5 p-2"
        ),
        toolbar_args
      )
    ),
    do.call(div, block_notifications),
    div(
      id = paste0(id, "_board"),
      do.call(div, c(id = paste0(id, "_blocks"), block_ui(id, x)))
    )
  )
}

#' @param blocks (Additional) blocks (or IDs) for which to generate the UI
#' @rdname block_ui
#' @export
block_ui.board <- function(id, x, blocks = NULL, ...) {

  block_card <- function(x, id, ns) {
    bslib::card(
      id = paste0(id, "_block"),
      bslib::card_header(
        paste0(block_name(x), " (", id, ")")
      ),
      bslib::card_body(
        expr_ui(ns(id), x)
      ),
      bslib::card_body(
        block_ui(ns(id), x)
      )
    )
  }

  stopifnot(is_string(id))

  if (is.null(blocks)) {
    blocks <- board_blocks(x)
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

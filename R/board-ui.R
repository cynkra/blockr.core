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

  plugins <- as_plugins(plugins)

  toolbar_plugins <- c("preserve_board", "manage_blocks", "manage_links",
                       "manage_stacks", "generate_code")

  toolbar_plugins <- plugins[intersect(toolbar_plugins, names(plugins))]

  toolbar_ui <- do.call(
    tagList,
    list(
      board_ui(id, toolbar_plugins, x),
      board_ui(id, board_options(x))
    )
  )

  if ("edit_block" %in% names(plugins)) {
    block_plugin <- plugins[["edit_block"]]
  } else {
    block_plugin <- NULL
  }

  if ("edit_stack" %in% names(plugins)) {
    stack_plugin <- plugins[["edit_stack"]]
  } else {
    stack_plugin <- NULL
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
        toolbar_ui
      )
    ),
    if ("notify_user" %in% names(plugins)) {
      div(board_ui(id, plugins[["notify_user"]], x))
    },
    div(
      id = paste0(id, "_board"),
      stack_ui(id, x, edit_ui = stack_plugin),
      block_ui(id, x, edit_ui = block_plugin)
    )
  )
}

#' @rdname board_ui
#' @export
board_ui.NULL <- function(id, x, ...) NULL

#' @section Board-level block UI:
#' While the contents of block-level UI are created by dispatching `block_ui()`
#' on blocks another dispatch on [`board`](new_board) occurs as well. This can
#' be used to control how blocks are integrated into the board UI. For the
#' default board, this uses [bslib::card()] to represent blocks. For boards
#' that extend the default `board` class, control is available for how blocks
#' are displayed by providing a board-specific `block_ui()` method.
#'
#' @param blocks (Additional) blocks (or IDs) for which to generate the UI
#' @param edit_ui Block edit plugin
#'
#' @rdname block_ui
#' @export
block_ui.board <- function(id, x, blocks = NULL, edit_ui = NULL, ...) {

  block_card <- function(x, block_id, board_ns, card_elems) {

    blk_id <- board_ns(paste0("block_", block_id))

    bslib::card(
      id = paste0(block_id, "_block"),
      card_elems(
        x,
        NS(blk_id, "edit_block"),
        bslib::card_body(
          expr_ui(blk_id, x),
          block_ui(blk_id, x)
        )
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

  if (is.null(edit_ui)) {
    edit_ui <- function(x, id, ...) {
      tagList(
        bslib::card_header(block_name(x)),
        ...
      )
    }
  } else {
    edit_ui <- get_plugin_ui(edit_ui)
  }

  args <- list(
    board_ns = NS(id),
    card_elems = edit_ui
  )

  do.call(
    div,
    c(
      id = paste0(id, "_blocks"),
      map(block_card, blocks, names(blocks), MoreArgs = args)
    )
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
    block_ui(id, x, blocks, ...),
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

    stopifnot(is.character(blocks))

    for (block in blocks) {
      removeUI(
        paste0("#", block, "_block"),
        immediate = TRUE
      )
    }
  }
}

#' @param session Shiny session
#' @rdname board_ui
#' @export
update_ui <- function(x, session, ...) {
  UseMethod("update_ui", x)
}

#' @rdname board_ui
#' @export
update_ui.board <- function(x, session, ...) {
  update_ui(board_options(x), session)
}

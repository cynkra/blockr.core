#' Board server
#'
#' Shiny server function for `board` objects.
#'
#' @param x Board
#' @param id (Optional) parent namespace
#' @param ... Generic consistency
#'
#' @export
board_server <- function(id, x, ...) {
  UseMethod("board_server", x)
}

#' @param ser_deser Module for serialization/deserialization
#' @param add_rm_block Module for addition/removal of blocks
#' @param add_rm_link Module for addition/removal of links between blocks
#' @param block_notifications Module for block nptifications
#' @rdname board_server
#' @export
board_server.board <- function(id,
                               x,
                               ser_deser = NULL,
                               add_rm_block = NULL,
                               add_rm_link = NULL,
                               block_notifications = NULL,
                               ...) {

  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveValues(
        blocks = list(),
        inputs = list(),
        board = x,
        board_id = id,
        links = list()
      )

      observeEvent(
        TRUE,
        {
          rv <- setup_blocks(rv)
        },
        once = TRUE
      )

      if (not_null(ser_deser)) {

        board_refresh <- check_ser_deser_val(
          ser_deser("ser_deser", rv)
        )

        observeEvent(
          board_refresh(),
          {
            remove_block_ui(id, rv$board)

            rv$board <- board_refresh()

            insert_block_ui(id, rv$board)

            rv <- setup_blocks(rv)
          }
        )
      }

      if (not_null(add_rm_block)) {

        blocks <- check_add_rm_block_val(
          add_rm_block("add_rm_block", rv),
          rv
        )

        observeEvent(
          blocks$add,
          {
            insert_block_ui(id, rv$board, blocks$add)

            board_blocks(rv$board) <- c(board_blocks(x), blocks$add)

            for (blk in names(blocks$add)) {
              rv <- setup_block(blocks$add[[blk]], blk, rv)
            }
          }
        )

        observeEvent(
          blocks$rm,
          {
            remove_block_ui(id, rv$board, blocks$rm)

            rv <- destroy_rm_blocks(blocks$rm, rv)
          }
        )
      }

      if (not_null(add_rm_link)) {

        links <- check_add_rm_link_val(
          add_rm_link("add_rm_link", rv),
          rv
        )

        observeEvent(
          links(),
          {
            updates <- links()

            rm <- board_links(rv$board)[updates$rm]
            rv <- update_block_links(rv, updates$add, rm)

            rv$board <- modify_links(rv$board, updates$add, updates$rm)
          },
          ignoreInit = TRUE
        )
      }

      if (is.null(block_notifications)) {
        notifications <- reactive(
          filter_all_zero_len(lst_xtr_reval(rv$blocks, "server", "cond"))
        )
      } else {
        notifications <- check_block_notifications_val(
          block_notification_server("block_notifications", rv)
        )
      }

      list(
        board = reactive(rv$board),
        blocks = reactive(rv$blocks),
        notifications = notifications
      )
    }
  )
}

setup_blocks <- function(rv) {

  stopifnot(
    is.reactivevalues(rv),
    all(c("blocks", "inputs", "board", "links") %in% names(rv)),
    is_board(rv$board)
  )

  for (link in rv$links) {
    link$destroy()
  }

  rv$blocks <- list()
  rv$inputs <- list()
  rv$links <- list()

  blks <- sort(rv$board)

  for (i in names(blks)) {
    rv <- setup_block(blks[[i]], i, rv)
  }

  rv
}

setup_block <- function(blk, id, rv) {

  rv$inputs[[id]] <- set_names(
    replicate(block_arity(blk), reactiveVal()),
    block_inputs(blk)
  )

  links <- board_links(rv$board)

  todo <- as.list(links[links$to == id])

  for (i in names(todo)) {
    rv <- do.call(setup_link, c(list(rv, i), todo[[i]]))
  }

  rv$blocks[[id]] <- list(
    block = blk,
    server = block_server(id, blk, rv$inputs[[id]])
  )

  rv
}

destroy_rm_blocks <- function(ids, rv) {

  links <- board_links(rv$board)
  blocks <- board_blocks(rv$board)

  rv <- update_block_links(
    rv,
    rm = links[links$from %in% ids | links$to %in% ids]
  )

  rv$inputs <- rv$inputs[!names(rv$inputs) %in% ids]
  rv$blocks <- rv$blocks[!names(rv$blocks) %in% ids]

  board_links(rv$board) <- links[!links$from %in% ids & !links$to %in% ids]
  board_blocks(rv$board) <- blocks[!names(blocks) %in% ids]

  rv
}

setup_link <- function(rv, id, from, to, input) {

  rv$links[[id]] <- observeEvent(
    rv$blocks[[from]]$server$result(),
    {
      rv$inputs[[to]][[input]](
        rv$blocks[[from]]$server$result()
      )
    },
    ignoreNULL = FALSE
  )

  rv
}

destroy_link <- function(rv, id, from, to, input) {

  rv$links[[id]]$destroy()
  rv$links[[id]] <- NULL

  rv$inputs[[to]][[input]](NULL)

  rv
}

update_block_links <- function(rv, add = NULL, rm = NULL) {

  todo <- as.list(rm)

  for (i in names(todo)) {
    rv <- do.call(destroy_link, c(list(rv, i), todo[[i]]))
  }

  todo <- as.list(add)

  for (i in names(todo)) {
    rv <- do.call(setup_link, c(list(rv, i), todo[[i]]))
  }

  rv
}

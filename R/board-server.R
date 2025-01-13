#' Board server
#'
#' Shiny server function for `board` objects.
#'
#' @param x Board
#' @param ... Generic consistency
#'
#' @export
board_server <- function(x, ...) {
  UseMethod("board_server")
}

#' @param ser_deser Module for serialization/deserialization
#' @param add_rm_block Module for addition/removal of blocks
#' @param add_rm_conn Module for addition/removal of connections between blocks
#' @rdname board_server
#' @export
board_server.board <- function(x,
                               ser_deser = NULL,
                               add_rm_block = NULL,
                               add_rm_conn = NULL,
                               ...) {
  moduleServer(
    board_id(x),
    function(input, output, session) {

      rv <- reactiveValues(
        blocks = list(),
        inputs = list(),
        board = x,
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
          ser_deser(rv)
        )

        observeEvent(
          board_refresh(),
          {
            removeUI(
              paste0("#", board_id(rv$board), "_blocks > div")
            )

            rv$board <- board_refresh()

            insertUI(
              paste0("#", board_id(rv$board), "_blocks"),
              "afterBegin",
              block_cards(rv$board)
            )

            rv <- setup_blocks(rv)
          }
        )
      }

      if (not_null(add_rm_block)) {

        block <- check_add_rm_block_val(
          add_rm_block(rv),
          rv
        )

        observeEvent(
          block$add,
          {
            insertUI(
              paste0("#", board_id(rv$board), "_blocks"),
              "beforeEnd",
              block_card(block$add, board_id(rv$board))
            )

            rv$board <- add_block(rv$board, block$add)

            rv <- setup_block(block$add, rv)
          }
        )

        observeEvent(
          block$rm,
          {
            removeUI(
              paste0("#", block$rm, "_block")
            )

            rv <- destroy_block(block$rm, rv)
          }
        )
      }

      if (not_null(add_rm_conn)) {

        conn <- check_add_rm_conn_val(
          add_rm_conn(rv),
          rv
        )

        observeEvent(
          conn(),
          {
            updates <- conn()

            old <- board_links(rv$board)

            rv <- update_block_connections(
              rv,
              add = updates$add,
              rm = old[old$id %in% updates$rm, ]
            )

            rv$board <- modify_links(rv$board, updates$add, updates$rm)
          },
          ignoreInit = TRUE
        )
      }

      list(
        board = reactive(rv$board),
        blocks = reactive(rv$blocks)
      )
    }
  )
}

setup_blocks <- function(rv) {

  stopifnot(
    is.reactivevalues(rv),
    setequal(names(rv), c("blocks", "inputs", "board", "links")),
    is_board(rv$board)
  )

  for (link in rv$links) {
    link$destroy()
  }

  rv$blocks <- list()
  rv$inputs <- list()
  rv$links <- list()

  for (blk in sort(rv$board)) {
    rv <- setup_block(blk, rv)
  }

  rv
}

setup_block <- function(blk, rv) {

  id <- block_uid(blk)

  rv$inputs[[id]] <- set_names(
    replicate(block_arity(blk), reactiveVal()),
    block_inputs(blk)
  )

  links <- board_links(rv$board)

  for (i in which(links$to == id)) {
    rv <- do.call(setup_connection, c(list(rv), links[i, ]))
  }

  rv$blocks[[id]] <- list(
    block = blk,
    server = block_server(
      blk,
      data = rv$inputs[[id]]
    )
  )

  rv
}

destroy_block <- function(id, rv) {

  links <- board_links(rv$board)

  for (row in which(links$from %in% id | links$to %in% id)) {
    rv <- do.call(destroy_connection, c(list(rv), links[row, ]))
  }

  rv$inputs[[id]] <- NULL
  rv$blocks[[id]] <- NULL

  rv$board <- remove_blocks(rv$board, id)

  rv
}

setup_connection <- function(rv, id, from, to, input) {

  rv$links[[id]] <- observeEvent(
    rv$blocks[[from]]$server$result(),
    {
      rv$inputs[[to]][[input]](
        rv$blocks[[from]]$server$result()
      )
    }
  )

  rv
}

destroy_connection <- function(rv, id, from, to, input) {

  rv$links[[id]]$destroy()
  rv$links[[id]] <- NULL

  rv$inputs[[to]][[input]](NULL)

  rv
}

update_block_connections <- function(rv, add = NULL, rm = NULL) {

  if (not_null(rm)) {
    for (i in seq_len(nrow(rm))) {
      rv <- do.call(destroy_connection, c(list(rv), rm[i, ]))
    }
  }

  if (not_null(add)) {
    for (i in seq_len(nrow(add))) {
      rv <- do.call(setup_connection, c(list(rv), add[i, ]))
    }
  }

  rv
}

#' Board server
#'
#' Shiny server function for `board` objects.
#'
#' @param x Board
#'
#' @export
board_server <- function(x) {
  UseMethod("board_server")
}

#' @rdname board_server
#' @export
board_server.board <- function(x) {
  moduleServer(
    attr(x, "id"),
    function(input, output, session) {

      rv <- reactiveValues(
        blocks = list(),
        inputs = list(),
        board = x
      )

      observeEvent(
        TRUE,
        {
          rv <- setup_blocks(rv)
        },
        priority = 100,
        once = TRUE
      )

      output$serialize <- downloadHandler(
        board_filename(rv$board),
        write_board_to_disk(rv$board, rv)
      )

      observeEvent(input$restore, {

        removeUI(
          paste0("#", attr(rv$board, "id"), "_blocks", " > div")
        )

        rv$board <- from_json(
          readLines(input$restore$datapath)
        )

        insertUI(
          paste0("#", attr(rv$board, "id"), "_blocks"),
          ui = block_cards(rv$board)
        )

        rv <- setup_blocks(rv)
      })

      list(
        board = reactive(rv$board),
        blocks = reactive(rv$blocks)
      )
    }
  )
}

board_filename <- function(x) {
  function() {
    paste0(
      attr(x, "id"), "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".json"
    )
  }
}

write_board_to_disk <- function(x, rv) {

  function(con) {

    blocks <- lapply(
      lapply(lapply(rv$blocks, `[[`, "server"), `[[`, "json"),
      reval
    )

    json <- jsonlite::prettify(
      to_json(x, blocks)
    )

    writeLines(json, con)
  }
}

setup_blocks <- function(rv) {

  stopifnot(
    is.reactivevalues(rv),
    setequal(names(rv), c("blocks", "inputs", "board")),
    is_board(rv$board)
  )

  rv$blocks <- list()
  rv$inputs <- list()

  for (blk in sort(rv$board)) {
    rv <- setup_block(blk, rv, rv$board[["links"]])
  }

  rv
}

setup_block <- function(blk, rv, links) {

  id <- block_uid(blk)

  if (block_arity(blk)) {
    rv$inputs[[id]] <- lapply(
      set_names(links$from[links$to == id], links$input[links$to == id]),
      function(src) rv$blocks[[src]]$server$result
    )
  } else {
    rv$inputs[[id]] <- list()
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

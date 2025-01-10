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
          "afterBegin",
          block_cards(rv$board)
        )

        rv <- setup_blocks(rv)
      })

      observeEvent(input$add_block, {

        req(input$block_select)

        blk <- create_block(input$block_select)

        insertUI(
          paste0("#", attr(rv$board, "id"), "_blocks"),
          "beforeEnd",
          block_card(blk, attr(rv$board, "id"))
        )

        rv$board <- add_block(rv$board, blk)

        rv <- setup_block(blk, rv)
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
    rv <- setup_block(blk, rv)
  }

  rv
}

setup_block <- function(blk, rv) {

  id <- block_uid(blk)

  links <- rv$board[["links"]]

  if (block_arity(blk)) {

    hits <- links$to == id

    if (sum(hits)) {

      rv$inputs[[id]] <- lapply(
        set_names(links$from[hits], links$input[hits]),
        function(src) rv$blocks[[src]]$server$result
      )

    } else {

      rv$inputs[[id]] <- set_names(
        rep(list(function() list()), block_arity(blk)),
        block_inputs(blk)
      )
    }

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

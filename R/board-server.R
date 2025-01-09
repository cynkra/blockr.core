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
        inputs = list()
      )

      observeEvent(
        TRUE,
        {
          for (blk in sort(x)) {
            rv <- setup_block(blk, rv, x[["links"]])
          }
        },
        once = TRUE
      )

      output$serialize <- downloadHandler(
        board_filename(x),
        write_board_to_disk(x, rv)
      )

      observeEvent(input$restore, {

        removeUI(
          selector = paste0("#", attr(x, "id"), "_blocks", " > div"),
          session = session
        )

        x <<- from_json(
          readLines(input$restore$datapath)
        )

        insertUI(
          selector = paste0("#", attr(x, "id"), "_blocks"),
          ui = block_cards(x),
          session = session
        )

        rv$block <- list()
        rv$input <- list()

        for (blk in sort(x)) {
          rv <- setup_block(blk, rv, x[["links"]])
        }
      })

      reactive(rv$blocks)
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

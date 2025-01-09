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
    }
  )
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

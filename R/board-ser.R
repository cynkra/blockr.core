#' Serialization module
#'
#' Object (de)serialization in a board server context.
#'
#' @param rv Reactive values object
#'
#' @rdname ser_deser
#' @export
ser_deser_server <- function(rv) {
  moduleServer(
    "ser_deser",
    function(input, output, session) {

      output$serialize <- downloadHandler(
        board_filename(rv$board),
        write_board_to_disk(rv$board, rv)
      )

      res <- reactiveVal()

      observeEvent(input$restore, {
        res(
          from_json(
            readLines(input$restore$datapath)
          )
        )
      })

      res
    }
  )
}

#' @param rv Namespace ID
#' @param board The initial `board` object
#' @rdname ser_deser
#' @export
ser_deser_ui <- function(id, board) {

  ns <- NS(
    NS(id, "ser_deser")
  )

  list(
    downloadButton(
      ns("serialize"),
      "Save",
      class = "mx-2"
    ),
    fileInput(
      ns("restore"),
      "Restore"
    )
  )
}

board_filename <- function(x) {
  function() {
    paste0(
      board_id(x), "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".json"
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

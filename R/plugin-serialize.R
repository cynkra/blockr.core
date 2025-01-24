#' Serialization module
#'
#' Object (de)serialization in a board server context.
#'
#' @param id Namespace ID
#' @param rv Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @return A [shiny::reactiveVal()] object that evaluates to `NULL` or a
#' `board` obejct.
#'
#' @rdname ser_deser
#' @export
ser_deser_server <- function(id, rv, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      output$serialize <- downloadHandler(
        board_filename(rv),
        write_board_to_disk(rv)
      )

      res <- reactiveVal()

      observeEvent(input$restore, {
        res(
          from_json(input$restore$datapath)
        )
      })

      res
    }
  )
}

#' @param board The initial `board` object
#' @rdname ser_deser
#' @export
ser_deser_ui <- function(id, board) {
  tagList(
    downloadButton(
      NS(id, "serialize"),
      "Save",
      class = "mx-2"
    ),
    fileInput(
      NS(id, "restore"),
      "Restore"
    )
  )
}

board_filename <- function(rv) {
  function() {
    paste0(
      rv$board_id, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".json"
    )
  }
}

write_board_to_disk <- function(rv) {

  function(con) {

    blocks <- lapply(
      lst_xtr(rv$blocks, "server", "state"),
      lapply,
      reval_if
    )

    json <- jsonlite::prettify(
      to_json(rv$board, blocks)
    )

    writeLines(json, con)
  }
}

check_ser_deser_val <- function(val) {

  observeEvent(
    TRUE,
    {
      if (!is.reactive(val)) {
        stop("Expecting a `ser_deser` server to return a reactive value.")
      }
    },
    once = TRUE
  )

  observeEvent(
    val(),
    {
      if (!is_board(val())) {
        stop("Expecting the `ser_deser` return value to evaluate to a ",
             "`board` object.")
      }

      validate_board(val())
    },
    once = TRUE
  )

  val
}

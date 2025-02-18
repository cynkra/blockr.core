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
        write_board_to_disk(rv, session)
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
      rv$board_id,
      "_",
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
      ".json"
    )
  }
}

write_board_to_disk <- function(rv, session) {

  function(con) {

    blocks <- lapply(
      lst_xtr(rv$blocks, "server", "state"),
      lapply,
      reval_if
    )

    opts <- lapply(
      set_names(nm = list_board_options(rv$board)),
      board_option_from_userdata,
      session
    )

    json <- jsonlite::prettify(
      to_json(rv$board, blocks, opts)
    )

    writeLines(json, con)
  }
}

check_ser_deser_val <- function(val) {
  observeEvent(
    TRUE,
    {
      if (!is.reactive(val)) {
        abort(
          "Expecting `preserve_board` to return a reactive value.",
          class = "preserve_board_return_invalid"
        )
      }
    },
    once = TRUE
  )

  observeEvent(
    val(),
    {
      if (!is_board(val())) {
        abort(
          paste(
            "Expecting the `preserve_board` return value to evaluate to a",
            "`board` object."
          ),
          class = "preserve_board_return_invalid"
        )
      }

      validate_board(val())
    },
    once = TRUE
  )

  val
}

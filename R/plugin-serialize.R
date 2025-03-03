#' Serialization module
#'
#' Object (de)serialization in a board server context.
#'
#' @param id Namespace ID
#' @param board Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @return A [shiny::reactiveVal()] object that evaluates to `NULL` or a
#' `board` obejct.
#'
#' @rdname ser_deser
#' @export
ser_deser_server <- function(id, board, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      output$serialize <- downloadHandler(
        board_filename(board),
        write_board_to_disk(board, session)
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
    htmltools::tagQuery(
      fileInput(
        NS(id, "restore"),
        "",
        buttonLabel = tagList(icon("upload"), "Restore")
      )
    )$addAttrs(
      style = "margin-bottom: 8px;"
    )$allTags()
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

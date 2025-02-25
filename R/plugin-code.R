#' Code generation module
#'
#' Generate reproducible code from a board.
#'
#' @param id Namespace ID
#' @param rv Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @return NULL (invisibly)
#'
#' @rdname gen_code
#' @export
gen_code_server <- function(id, rv, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      code <- reactive(
        generate_code(rv)
      )

      observeEvent(
        input$code_mod,
        {
          output$code_out <- renderPrint(HTML(code()))

          id <- "code_out"

          showModal(
            modalDialog(
              title = "Generated code",
              div(
                class = "text-decoration-none position-relative",
                if (nchar(code())) copy_to_clipboard(session, id),
                verbatimTextOutput(session$ns(id))
              ),
              easyClose = TRUE,
              footer = NULL,
              size = "l"
            )
          )
        }
      )

      invisible(NULL)
    }
  )
}

#' @param board The initial `board` object
#' @rdname gen_code
#' @export
gen_code_ui <- function(id, board) {
  tagList(
    actionButton(
      NS(id, "code_mod"),
      "Show code",
      icon = icon("code")
    )
  )
}

copy_to_clipboard <- function(session, id) {

  deps <- htmltools::htmlDependency(
    "copy-to-clipboard",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "copyToClipboard.js"
  )

  tagList(
    actionButton(
      session$ns("copy_code"),
      "",
      class = paste(
        "btn", "btn-outline-secondary", "btn-sm", "position-absolute",
        "top-0", "end-0", "m-2"
      ),
      icon = icon("copy", c("fa-solid", "fa-2x")),
      onclick = paste0("copyCode(\"", session$ns(id), "\");")
    ),
    deps
  )
}

check_gen_code_val <- function(val) {

  observeEvent(
    TRUE,
    {
      if (!is.null(val)) {
        abort(
          "Expecting `generate_code` to return `NULL`.",
          class = "generate_code_return_invalid"
        )
      }
    },
    once = TRUE
  )

  invisible(val)
}

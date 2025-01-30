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

      observeEvent(
        input$code,
        {
          output$code <- renderPrint(HTML(generate_code(rv)))

          showModal(
            modalDialog(
              title = "Generated code",
              div(
                class = "text-decoration-none",
                verbatimTextOutput(session$ns("code"))
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
      NS(id, "code"),
      "Show code",
      icon = icon("code")
    )
  )
}

generate_code <- function(rv) {

  exprs <- lapply(lst_xtr(rv$blocks, c("server", "expr")), reval)

  lnks <- board_links(rv$board)

  arg_map <- lapply(
    split(as.list(lnks), lnks$to),
    function(x) {
      set_names(lapply(lst_xtr(x, "from"), as.name), lst_xtr(x, "input"))
    }
  )

  exprs <- Map(substitute_expr, exprs, arg_map[names(exprs)])

  exprs <- map(assignment, names(exprs), exprs)
  exprs <- lapply(exprs, deparse)
  exprs <- chr_ply(exprs, paste0, collapse = "\n")

  paste0(exprs, collapse = "\n\n")
}

check_gen_code_val <- function(val) {

  if (!is.null(val)) {
    stop("Expecting a `gen_code` server to return `NULL`.")
  }

  invisible(val)
}

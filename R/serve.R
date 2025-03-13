#' Serve object
#'
#' Start up shiny app.
#'
#' @param x Object
#' @param ... Generic consistency
#'
#' @export
serve <- function(x, ...) {
  UseMethod("serve")
}

#' @param id Block ID
#' @param data Data inputs
#' @rdname serve
#' @export
serve.block <- function(x, id = "block", ..., data = list()) {

  init_data <- function(x, is_variadic) {
    if (is_variadic) do.call(reactiveValues, x) else reactiveVal(x)
  }

  if (...length() && !length(data)) {
    data <- list(...)
  }

  dot_args <- !names(data) %in% block_inputs(x)

  if (!is.na(block_arity(x)) && any(dot_args)) {
    stop("Unexpected arguments.")
  }

  if (any(dot_args)) {
    data <- c(data[!dot_args], list(...args = data[dot_args]))
  }

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    title = id,
    expr_ui(id, x),
    block_ui(id, x)
  )

  server <- function(input, output, session) {

    res <- block_server(id, x, Map(init_data, data, names(data) == "...args"))

    exportTestValues(
      result = safely_export(res$result())()
    )

    invisible()
  }

  shinyApp(ui, server)
}

#' @param id Board namespace ID
#' @param plugins Board plugins
#' @rdname serve
#' @export
serve.board <- function(x, id = rand_names(), plugins = board_plugins(), ...) {

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    title = board_option("board_name", x),
    board_ui(id, x, plugins),
    htmltools::htmlDependency(
      "change-board-title",
      pkg_version(),
      src = pkg_file("assets", "js"),
      script = "changeBoardTitle.js"
    )
  )

  server <- function(input, output, session) {

    observeEvent(
      board_option_from_userdata("board_name", session),
      session$sendCustomMessage(
        "change-board-title",
        board_option_from_userdata("board_name", session)
      )
    )

    res <- board_server(id, x, plugins)

    exportTestValues(
      result = lapply(
        lapply(
          lapply(lst_xtr(res[[1L]]$blocks, "server", "result"), safely_export),
          reval
        ),
        reval
      )
    )

    invisible()
  }

  shinyApp(ui, server)
}

#' Board options
#'
#' User settings at the board level.
#'
#' @param board_name String valued board name
#' @param n_rows,page_size Numer of rows and page size to show for tabular
#' block previews
#' @param filter_rows Enable filtering of rows in tabular block previews
#' @param dark_mode Toggle between dark and light modes
#' @param ... Further options
#' @param class Optional sub-class
#'
#' @export
new_board_options <- function(board_name = "Board",
                              n_rows = get_option("n_rows", 50L),
		    											page_size = get_option("page_size", 5L),
                              filter_rows = get_option("filter_rows", FALSE),
                              dark_mode = get_option("dark_mode", NULL),
                              ...,
                              class = character()) {

  res <- structure(
    list(
      board_name = board_name,
      n_rows = as.integer(n_rows),
      page_size = as.integer(page_size),
      filter_rows = filter_rows,
      dark_mode = dark_mode,
      ...
    ),
    class = c(class, "board_options")
  )

  validate_board_options(res)
}

#' @param x Board options object
#' @rdname new_board_options
#' @export
is_board_options <- function(x) {
  inherits(x, "board_options")
}

#' @rdname new_board_options
#' @export
as_board_options <- function(x) {
  UseMethod("as_board_options", x)
}

#' @rdname new_board_options
#' @export
as_board_options.board_options <- function(x) {
  x
}

#' @rdname new_board_options
#' @export
as_board_options.default <- function(x) {
  validate_board_options(
    structure(as.list(x), class = "board_options")
  )
}

#' @rdname new_board_options
#' @export
validate_board_options <- function(x) {
  UseMethod("validate_board_options", x)
}

#' @rdname new_board_options
#' @export
validate_board_options.board_options <- function(x) {

  if (!is_board_options(x)) {
    abort(
      "Expecting board options to inherit from `board_options`.",
      class = "board_options_inheritance_invalid"
    )
  }

  expected <- c(
    "board_name", "n_rows", "page_size", "filter_rows", "dark_mode"
  )

  if (!all(expected %in% names(x))) {
    abort(
      paste(
        "Expecting", paste_enum(setdiff(expected, names(x))),
        "to be available as board options."
      ),
      class = "board_options_components_invalid"
    )
  }

  if (!is_string(x[["board_name"]])) {
    abort(
      "Expecting `board_name` to represent a string.",
      class = "board_options_board_name_invalid"
    )
  }

  if (!is_count(x[["n_rows"]])) {
    abort(
      "Expecting `n_rows` to represent a count.",
      class = "board_options_n_rows_invalid"
    )
  }

  if (!is_count(x[["page_size"]])) {
    abort(
      "Expecting `page_size` to represent a count.",
      class = "board_options_page_size_invalid"
    )
  }

  if (!is_bool(x[["filter_rows"]])) {
    abort(
      "Expecting `filter_rows` to represent a boolean.",
      class = "board_options_filter_rows_invalid"
    )
  }

  dm <- x[["dark_mode"]]

  if (!(is.null(dm) || (is_string(dm) && dm %in% c("light", "dark")))) {
    abort(
      "Expecting `dark_mode` to be either `NULL`, \"light\" or \"dark\".",
      class = "board_options_dark_mode_invalid"
    )
  }

  x
}

#' @export
as.list.board_options <- function(x, ...) {
  unclass(x)
}

#' @rdname new_board_options
#' @export
list_board_options <- function(x) {

  if (is_board(x)) {
    x <- board_options(x)
  }

  if (!is_board_options(x)) {
    abort(
      "Expecting a `board_options` object to be passed as `x`.",
      class = "board_option_x_invalid"
    )
  }

  names(x)
}

#' @param opt Board option
#' @rdname new_board_options
#' @export
board_option <- function(opt, x) {

  if (is_board(x)) {
    x <- board_options(x)
  }

  if (!is_board_options(x)) {
    abort(
      "Expecting a `board_options` object to be passed as `x`.",
      class = "board_option_x_invalid"
    )
  }

  if (!is_string(opt)) {
    abort(
      "Expecting a string to be passed as `opt`.",
      class = "board_option_opt_invalid"
    )
  }

  if (!opt %in% list_board_options(x)) {
    abort(
      "Expecting `opt` to be a component of `x`.",
      class = "board_option_opt_invalid"
    )
  }

  x[[opt]]
}

#' @rdname board_ui
#' @export
board_ui.board_options <- function(id, x, ...) {

  ns <- NS(id)

  bslib::popover(
    bsicons::bs_icon("gear", size = "1.5em"),
    textInput(
      ns("board_name"),
      "Board name",
      board_option("board_name", x)
    ),
    numericInput(
      ns("n_rows"),
      "Preview rows",
      board_option("n_rows", x),
      min = 1L,
      step = 1L
    ),
    selectInput(
      ns("page_size"),
      "Preview page size",
      c(5, 10, 25, 50, 100),
      board_option("page_size", x)
    ),
    bslib::input_switch(
      ns("filter_rows"),
      "Enable preview search",
      board_option("filter_rows", x)
    ),
    span(
      bslib::input_dark_mode(
        id = ns("dark_mode"),
        mode = board_option("dark_mode", x)
      ),
      tags$label(
        "Light/dark mode",
        style = "vertical-align: top; margin-top: 3px;"
      )
    ),
    title = "Board options"
  )
}

#' @rdname board_ui
#' @export
update_ui.board_options <- function(x, session, ...) {

  updateTextInput(
    session,
    "board_name",
    value = board_option("board_name", x)
  )

  updateNumericInput(
    session,
    "n_rows",
    value = board_option("n_rows", x)
  )

  updateSelectInput(
    session,
    "page_size",
    selected = board_option("page_size", x)
  )

  bslib::toggle_switch(
    "filter_rows",
    value = board_option("filter_rows", x),
    session = session
  )

  bslib::toggle_dark_mode(
    mode = board_option("dark_mode", x),
    session = session
  )

  invisible()
}

board_option_to_userdata <- function(x, input, session) {
  session$userData[[x]] <- reactive(input[[x]])
  invisible()
}

board_options_to_userdata <- function(x, input, session) {

  if (is_board(x)) {
    x <- board_options(x)
  }

  lapply(list_board_options(x), board_option_to_userdata, input, session)

  invisible()
}

board_option_from_userdata <- function(name, session) {

  rv <- get0(name, envir = session$userData, inherits = FALSE)

  if (is.null(rv)) {
    return(NULL)
  }

  res <- rv()

  if (is.null(res)) {
    return(NULL)
  }

  if (identical(name, "page_size")) {
    res <- as.integer(res)
  }

  res
}

get_userdata_or_option <- function(name, session) {

  res <- board_option_from_userdata(name, session)

  if (is.null(res)) {
    return(board_option(name, new_board_options()))
  }

  res
}

#' @export
format.board_options <- function(x, ...) {
  trimws(utils::capture.output(utils::str(as.list(x)))[-1L], "right")
}

#' @export
print.board_options <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

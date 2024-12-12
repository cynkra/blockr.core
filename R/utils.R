is_scalar <- function(x) length(x) == 1L

is_string <- function(x) {
  is.character(x) && is_scalar(x)
}

is_bool <- function(x) {
  is_scalar(x) && (identical(x, TRUE) || identical(x, FALSE))
}

is_intish <- function(x) {
  is.integer(x) || (is.numeric(x) && all(x == trunc(x)) && !is.na(x))
}

is_count <- function(x, allow_zero = TRUE) {

  if (!is_scalar(x)) {
    return(FALSE)
  }

  if (!is_intish(x)) {
    return(FALSE)
  }

  if (isTRUE(allow_zero)) {
    x >= 0 && !is.na(x)
  } else {
    x > 0 && !is.na(x)
  }
}

is_number <- function(x) {
  is.numeric(x) && is_scalar(x) && !is.na(x) && !is.nan(x) && is.finite(x)
}

#' Random IDs
#'
#' Generate random unique IDs.
#'
#' @param old_names Disallowed IDs
#' @param n Number of IDs to generate
#' @param length ID length
#' @param chars Allowed characters
#' @param prefix,suffix ID pre-/suffix
#'
#' @export
rand_names <- function(old_names = character(0L), n = 1L, length = 15L,
                       chars = letters, prefix = "", suffix = "") {
  stopifnot(
    is.null(old_names) || is.character(old_names),
    is_count(n), is_count(length),
    is.character(chars), length(chars) >= 1L,
    is_string(prefix), is_string(suffix),
    nchar(prefix) + nchar(suffix) < length
  )

  length <- length - (nchar(prefix) + nchar(suffix))

  repeat {
    res <- replicate(
      n,
      paste0(
        prefix,
        paste(sample(chars, length, replace = TRUE), collapse = ""),
        suffix
      )
    )

    if (length(res) == length(unique(res)) && !any(res %in% old_names)) {
      break
    }
  }

  res
}

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

#' @rdname serve
#' @export
serve.character <- function(x, ...) {

  stopifnot(is_string(x))

  server <- function(input, output, session) {
    board_server(x)
  }

  shinyApp(board_ui(x), server)
}

chr_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, character(length), ..., USE.NAMES = use_names)
}

#' @keywords internal
lgl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, logical(length), ..., USE.NAMES = use_names)
}

int_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, integer(length), ..., USE.NAMES = use_names)
}

dbl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, double(length), ..., USE.NAMES = use_names)
}

chr_xtr <- function(x, i, ...) chr_ply(x, `[[`, i, ...)

lgl_xtr <- function(x, i, ...) lgl_ply(x, `[[`, i, ...)

int_xtr <- function(x, i, ...) int_ply(x, `[[`, i, ...)

dbl_xtr <- function(x, i, ...) dbl_ply(x, `[[`, i, ...)

lst_xtr <- function(x, i) lapply(x, `[[`, i)

map <- function(f, ..., use_names = FALSE) Map(f, ..., USE.NAMES = use_names)

not_null <- Negate(is.null)

reval <- function(x) x()

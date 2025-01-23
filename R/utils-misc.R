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

reval <- function(x) x()

reval_if <- function(x) if (is.function(x)) x() else x

lst_xtr_reval <- function(x, ...) {
  lapply(lst_xtr(x, ...), reval)
}

inherits <- function(x, ..., agg = NULL) {

  res <- lgl_ply(c(...), function(y) base::inherits(x, y))

  if (is.null(agg)) {
    return(res)
  }

  agg(res)
}

set_names <- function(object = nm, nm) {
  names(object) <- nm
  object
}

paste_enum <- function(x, sep = ", ", conj = " and ", quotes = "`") {

  if (length(x) == 0L) {
    return(x)
  }

  if (length(x) == 1L) {
    return(paste0(quotes, x, quotes))
  }

  paste0(
    paste0(quotes, x[seq_len(length(x) - 1L)], quotes, collapse = sep),
    conj,
    quotes, x[length(x)], quotes
  )
}

coal <- function(..., fail_null = TRUE) {

  for (i in seq_len(...length())) {
    x <- ...elt(i)
    if (is.null(x)) next else return(x)
  }

  if (isTRUE(fail_null)) {
    stop("No non-NULL value encountered")
  }

  NULL
}

filter_all_zero_len <- function(x) {
  if (all_zero_len(x)) {
    NULL
  } else if (is.list(x)) {
    Filter(Negate(is.null), lapply(x, filter_all_zero_len))
  } else {
    x
  }
}

int_to_chr <- function(x) {

  stopifnot(is_intish(x))

  chr_ply(
    lapply(strsplit(as.character(x), ""), as.integer),
    function(i) paste0(letters[i + 1L], collapse = "")
  )
}

filter_empty <- function(x) Filter(Negate(is_empty), x)

unlst <- function(x, recursive = FALSE, use_names = FALSE) {
  unlist(x, recursive = recursive, use.names = use_names)
}

na_to_empty <- function(x) replace(x, is.na(x), "")

get_option <- function(name, default) {

  opt <- tolower(paste0("blockr.", name))
  env <- toupper(paste0("blockr_", name))

  res_opt <- getOption(opt, default = default)
  res_env <- Sys.getenv(env, unset = "")

  if (identical(res_env, "")) {
    res_env <- default
  }

  if (identical(res_opt, res_env)) {
    return(res_opt)
  }

  if (identical(res_opt, default) && !identical(res_env, default)) {
    return(res_env)
  }

  if (identical(res_env, default) && !identical(res_opt, default)) {
    return(res_opt)
  }

  stop("Conflicting options set for ", name, ": check environment variable `",
       env, "` and option `", opt, "`.")
}

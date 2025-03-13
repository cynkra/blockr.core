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

int_to_chr <- function(x) {

  stopifnot(is_intish(x))

  chr_ply(
    lapply(strsplit(as.character(x), ""), as.integer),
    function(i) paste0(letters[i + 1L], collapse = "")
  )
}

unlst <- function(x, recursive = FALSE, use_names = FALSE) {
  unlist(x, recursive = recursive, use.names = use_names)
}

na_to_empty <- function(x) replace(x, is.na(x), "")

get_option <- function(name, default) {

  opt <- tolower(paste0("blockr.", name))
  env <- toupper(paste0("blockr_", name))

  res_opt <- getOption(opt, default = NULL)
  res_env <- Sys.getenv(env, unset = "")

  if (is.null(res_opt) && identical(res_env, "")) {
    return(default)
  }

  if (identical(res_opt, res_env)) {
    return(res_opt)
  }

  if (is.null(res_opt) && !identical(res_env, "")) {
    return(res_env)
  }

  if (identical(res_env, "") && !is.null(res_opt)) {
    return(res_opt)
  }

  stop("Conflicting options set for ", name, ": check environment variable `",
       env, "` and option `", opt, "`.")
}

dot_args_names <- function(x) {

  res <- names(x)

  unnamed <- grepl("^[1-9][0-9]*$", res)

  if (all(unnamed)) {
    return(NULL)
  }

  if (any(unnamed)) {
    return(replace(res, unnamed, ""))
  }

  res
}

dot_args_to_list <- function(x) {
  set_names(reactiveValuesToList(x), dot_args_names(x))
}

# https://github.com/rstudio/shiny/issues/3768
safely_export <- function(r) {
  r_quo <- rlang::enquo(r)
  rlang::inject({
    reactive({
      tryCatch(!!r_quo, error = function(e) e)
    })
  })
}

exprs_to_lang <- function(exprs) {

  if (rlang::is_syntactic_literal(exprs)) {
    return(exprs)
  }

  if (is.expression(exprs)) {
    exprs <- as.list(exprs)
  }

  if (is.list(exprs)) {
    exprs <- as.call(c(quote(`{`), exprs))
  }

  stopifnot(typeof(exprs) == "language")

  exprs
}

starts_with <- function(x, prefix) {
  x[startsWith(x, prefix)]
}

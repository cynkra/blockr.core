#' Board links
#'
#' Blocks on a board are linksed by board links.
#'
#' @param from,to Block ID(s)
#' @param input Block argument
#' @param ... Extensibility
#' @param class (Optional) link sub-class
#'
#' @export
new_link <- function(from = "", to = "", input = "", ...,
                     class = character()) {

  stopifnot(length(from) == length(to))

  if (length(from) && is_empty(input)) {
    input <- ""
  }

  validate_link(
    new_vctr(
      list(from = from, to = to, input = input, ...),
      class = c(class, "link")
    )
  )
}

validate_link <- function(x) {

  if (!is_link(x)) {
    stop("Expecting a link to inherit from \"link\".")
  }

  if (!is.list(x)) {
    stop("Expecting a link to be represented by a list.")
  }

  if (all_zero_len(x)) {
    return(x)
  }

  fields <- c("from", "to", "input")

  if (!all(fields %in% names(x))) {
    stop("Expecting a link to contain attributes ", paste_enum(fields), ".")
  }

  if (!all(lgl_ply(x[fields], is_string))) {
    stop("Expecting link attributes ", paste_enum(fields), " to be strings.")
  }

  if (anyNA(x[fields])) {
    stop("Missing values for ", paste_enum(fields), " are not allowed.")
  }

  if (x[["from"]] == x[["to"]] && x[["to"]] != "") {
    stop("Self-referencing links are not allowed.")
  }

  x
}

#' @rdname new_link
#' @export
is_link <- function(x) {
  inherits(x, "link")
}

#' @param x Links object
#' @rdname new_link
#' @export
as_link <- function(x) {
  UseMethod("as_link")
}

#' @rdname new_link
#' @export
as_link.link <- function(x) {
  x
}

#' @rdname new_link
#' @export
as_link.character <- function(x) {
  do.call(new_link, as.list(x))
}

#' @rdname new_link
#' @export
as_link.list <- function(x) {
  do.call(new_link, x)
}

#' @method as_link data.frame
#' @rdname new_link
#' @export
as_link.data.frame <- function(x) {
  do.call(new_link, x)
}

#' @export
as.character.link <- function(x, ...) {
  as.character(as.list(x))
}

#' @export
as.list.link <- function(x, ...) {
  vec_data(x)
}

#' @method as.data.frame link
#' @export
as.data.frame.link <- function(x, ...) {
  as.data.frame(as.list(x))
}

#' @export
`[.link` <- function(x, i, ...) {
  x <- vec_data(x)
  x[vec_as_location(i, length(x), names(x))]
}

#' @export
`[[.link` <- function(x, i, ...) {
  x <- vec_data(x)
  x[[vec_as_location2(i, length(x), names(x))]]
}

#' @export
format.link <- function(x, ...) {

  field_miss <- function(x) {
    if (is.na(x) || !nzchar(x)) "?" else x
  }

  out <- ""

  for (cl in rev(setdiff(class(x), c("list", "vctrs_vctr")))) {
    out <- paste0("<", cl, out, ">")
  }

  if (all_zero_len(x)) {
    return(out)
  }

  inp <- x["input"]

  if (is.na(inp) || !nzchar(inp)) {
    inp <- ""
  } else {
    inp <- paste0(" (", inp, ")")
  }

  c(out, paste0(field_miss(x["from"]), " -> ", field_miss(x["to"]), inp))
}

#' @export
print.link <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
c.link <- function(...) {
  as_links(lapply(list(...), as_link))
}

#' @export
vec_restore.link <- function(x, to, ...) {
  validate_link(NextMethod())
}

#' @export
vec_ptype2.link.link <- function(x, y, ...) x

#' @export
vec_ptype2.character.link <- function(x, y, ...) y

#' @export
vec_ptype2.link.character <- function(x, y, ...) x

#' @export
vec_ptype2.list.link <- function(x, y, ...) y

#' @export
vec_ptype2.link.list <- function(x, y, ...) x

#' @export
vec_cast.link.link <- function(x, to, ...) x

#' @export
vec_cast.link.character <- function(x, to, ...) as_link(x)

#' @export
vec_cast.character.link <- function(x, to, ...) as.character(x)

#' @export
vec_cast.link.list <- function(x, to, ...) as_link(x)

#' @export
vec_cast.list.link <- function(x, to, ...) as.list(x)

#' @export
vec_cast.link.data.frame <- function(x, to, ...) as_link(x)

#' @export
vec_cast.data.frame.link <- function(x, to, ...) as.data.frame(x)

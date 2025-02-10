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
    abort(
      "Expecting a link to inherit from \"link\".",
      class = "link_class_invalid"
    )
  }

  if (!is.list(x)) {
    abort(
      "Expecting a link to be represented by a list.",
      class = "link_list_like_invalid"
    )
  }

  if (all_zero_len(x)) {
    return(x)
  }

  fields <- c("from", "to", "input")

  if (!all(fields %in% names(x))) {
    abort(
      paste0(
        "Expecting a link to contain attributes ", paste_enum(fields), "."
      ),
      class = "link_components_missing"
    )
  }

  if (!all(lgl_ply(x[fields], is_string))) {
    abort(
      paste0(
        "Expecting link attributes ", paste_enum(fields), " to be strings."
      ),
      class = "link_components_invalid"
    )
  }

  if (anyNA(x[fields])) {
    abort(
      paste0(
        "Missing values for ", paste_enum(fields), " are not allowed."
      ),
      class = "link_components_invalid"
    )
  }

  if (x[["from"]] == x[["to"]] && x[["to"]] != "") {
    abort(
      "Self-referencing links are not allowed.",
      class = "link_self_referencing"
    )
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
  do.call(c, as.list(x))
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
  as_links(list_to_list_of_links(list(...)))
}

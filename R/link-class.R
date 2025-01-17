#' Board links
#'
#' Blocks on a board are linked by board links.
#'
#' @param from,to Block IDs
#' @param input Block argument
#' @param id Link ID
#' @param ... Extensibility
#'
#' @export
new_link <- function(from = character(), to = character(), input = NULL,
                     id = NULL, ...) {

  if (is.null(input) || all(is.na(input))) {
    input <- rep("", length(from))
  }

  if (is.null(id)) {
    if (length(from)) {
      id <- rand_names(n = length(from))
    } else {
      id <- character()
    }
  }

  vals <- list(id = id, from = from, to = to, input = input, ...)

  validate_link(
    vctrs::new_rcrd(vals, class = "link")
  )
}

#' @param x Link object
#' @rdname new_link
#' @export
is_link <- function(x) {
  inherits(x, "link")
}

#' @export
names.link <- function(x) {
  field(x, "id")
}

#' @export
`names<-.link` <- function(x, value) {

  if (is.null(value)) {
    value <- rep("", length(x))
  } else if (anyDuplicated(value) != 0L) {
    stop("IDs are required to be unique.")
  }

  field(x, "id") <- value

  x
}

#' @export
format.link <- function(x, ...) {

  field_miss <- function(x, field) {
    res <- field(x, field)
    replace(res, is.na(res) | !nzchar(res), "?")
  }

  input <- function(x) {
    res <- field(x, "input")
    ifelse(is.na(res) | !nzchar(res), "", paste0(" (", res, ")"))
  }

  paste0(field_miss(x, "from"), " -> ", field_miss(x, "to"), input(x))
}

#' @export
vec_ptype_abbr.link <- function(x, ...) {
  "link"
}

#' @export
vec_restore.link <- function(x, to, ...) {
  validate_link(NextMethod())
}

#' @export
vec_ptype2.link.link <- function(x, y, ...) x

#' @export
vec_ptype2.list.link <- function(x, y, ...) y

#' @export
vec_ptype2.link.list <- function(x, y, ...) x

#' @export
vec_ptype2.data.frame.link <- function(x, y, ...) y

#' @export
vec_ptype2.link.data.frame <- function(x, y, ...) x

#' @export
vec_cast.link.link <- function(x, to, ...) x

#' @export
vec_cast.link.list <- function(x, to, ...) as_link(x)

#' @export
vec_cast.list.link <- function(x, to, ...) as.list(x)

#' @export
vec_cast.link.data.frame <- function(x, to, ...) as_link(x)

#' @export
vec_cast.data.frame.link <- function(x, to, ...) as.data.frame(x)

#' @export
c.link <- function(...) {
  args <- lapply(list(...), as_link)
  do.call(link_c, c(args, list(.ptype = args[[1L]])))
}

#' @export
`[.link` <- function(x, i, ...) {
  link_slice(x, vec_as_location(i, length(x), names(x)))
}

#' @export
`[<-.link` <- function(x, i, ..., value) {

  i <- vec_as_location(i, length(x), names(x))

  if (is.null(value)) {
    return(link_slice(x, -i))
  }

  if (is.character(value)) {
    value <- as.list(value)
  }

  if (is.list(value)) {
    value <- list_to_df(value)
  }

  trg_ids <- field(link_slice(x, i), "id")

  if (is.data.frame(value)) {

    if (!"id" %in% colnames(value)) {
      value <- cbind(id = trg_ids, value)
    }

    value <- as_link(value)
  }

  stopifnot(is_link(value), setequal(trg_ids, field(value, "id")))

  link_assign(x, i, value[trg_ids])
}

#' @export
`[[.link` <- function(x, i, ...) {

  res <- vec_proxy(
    link_slice(x, vec_as_location2(i, length(x), names(x)))
  )

  as.list(res[, colnames(res) != "id"])
}

#' @export
`[[<-.link` <- function(x, i, ..., value) {

  i <- vec_as_location2(i, length(x), names(x))

  if (is.null(value)) {
    return(link_slice(x, -i))
  }

  if (is.character(value)) {
    value <- as.list(value)
  }

  if (is.list(value)) {
    value <- list_to_df(value)
  }

  stopifnot(
    is.data.frame(value), !"id" %in% colnames(value), nrow(value) == 1L
  )

  value <- as_link(
    cbind(id = field(link_slice(x, i), "id"), value)
  )

  link_assign(x, i, value)
}

#' @export
`$.link` <- function(x, name) {
  field(x, name)
}

#' @export
`$<-.link` <- function(x, name, value) {
  `field<-`(x, name, value)
}

#' @rdname new_link
#' @export
as_link <- function(x) {
  UseMethod("as_link")
}

#' @rdname new_link
#' @export
as_link.link <- function(x) {
  validate_link(x)
}

list_to_df <- function(x) {

  if (!length(x)) {
    return(data.frame())
  }

  if (is.atomic(x[[1L]])) {
    x <- list(x)
  }

  do.call(rbind.data.frame, x)
}

#' @rdname new_link
#' @export
as_link.list <- function(x) {
  as_link(list_to_df(x))
}

#' @method as_link data.frame
#' @rdname new_link
#' @export
as_link.data.frame <- function(x) {
  do.call(new_link, x)
}

#' @method as.data.frame link
#' @export
as.data.frame.link <- function(x, ...) {
  vec_proxy(x)
}

#' @export
as.list.link <- function(x, ...) {

  df <- vec_proxy(x)

  res <- split(df, seq_len(nrow(df)))
  res <- lapply(res, as.list)
  names(res) <- NULL

  res
}

link_slice <- function(...) {
  validate_link(vec_slice(...))
}

link_assign <- function(...) {
  validate_link(vec_assign(...))
}

link_c <- function(...) {
  validate_link(vec_c(...))
}

#' @rdname new_link
#' @export
validate_link <- function(x) {

  any_dup <- function(x) {
    anyDuplicated(Filter(Negate(is.na), x))
  }

  if (is_board(x)) {
    x <- board_links(x)
  }

  if (!is_link(x)) {
    stop("Expecting a boad link objects to inherit from `link`.")
  }

  fields <- c("id", "from", "to", "input")

  if (!all(fields %in% fields(x))) {
    stop(
      "Expecting the link data.frame to contain at least columns ",
      paste_enum(fields)
    )
  }

  for (field in fields) {
    if (!is.character(field(x, field))) {
      stop("Expecting field `", field, "` to be of type `character`.")
    }
  }

  self_ref <- field(x, "from") == field(x, "to") & field(x, "from") != ""

  if (any(!is.na(self_ref) & self_ref)) {
    stop("Self-referencing blocks are not allowed.")
  }

  if (any_dup(field(x, "id")) != 0L) {
    stop("Link IDs are required to be unique.")
  }

  dup_inp <- lapply(split(field(x, "input"), field(x, "to")), any_dup)
  dup_inp <- lgl_ply(dup_inp, Negate(identical), 0L, use_names = TRUE)

  if (any(dup_inp)) {
    stop("Block(s) ", paste_enum(names(dup_inp)[dup_inp]), " have ",
         "multiple identical inputs.")
  }

  x
}

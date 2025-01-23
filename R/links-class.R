#' @rdname new_link
#' @export
links <- function(...) {

  vals <- list_to_list_of_links(list(...))

  if (length(vals)) {

    if (is.null(names(vals))) {
      names(vals) <- rand_names(n = length(vals))
    }

    miss <- is.na(names(vals)) | names(vals) == ""

    if (any(miss)) {
      names(vals)[miss] <- rand_names(names(vals)[!miss], sum(miss))
    }

    ids <- names(vals)

  } else {

    vals <- list(lapply(new_link(), `[`, 0L))
    ids <- character()
  }

  res <- do.call(rbind, lapply(vals, as.data.frame))
  res <- cbind(id = ids, res)

  rownames(res) <- NULL

  validate_links(
    vctrs::new_rcrd(res, class = "links")
  )
}

list_to_list_of_links <- function(x) {

  if (length(x) == 1L && is.character(x[[1L]])) {
    x <- list(as.list(x))
  }

  if (any(lgl_ply(x, is.atomic))) {

    stopifnot(
      all(length(x[[1L]]) == lengths(x[-1L]))
    )

    vals <- lgl_ply(
      coal(names(x), rep("", length(x))),
      Negate(identical),
      "id"
    )

    res <- do.call(map, c(new_link, x[vals]))

    if ("id" %in% names(x)) {
      names(res) <- x[["id"]]
    } else {
      names(res) <- NULL
    }

    return(res)
  }

  lapply(x, as_link)
}

#' @rdname new_link
#' @export
is_links <- function(x) {
  inherits(x, "links")
}

#' @export
names.links <- function(x) {
  field(x, "id")
}

#' @export
`names<-.links` <- function(x, value) {

  if (is.null(value)) {
    value <- rep("", length(x))
  } else if (anyDuplicated(value) != 0L) {
    stop("IDs are required to be unique.")
  }

  field(x, "id") <- value

  x
}

#' @export
format.links <- function(x, ...) {
  res <- chr_ply(as.list(x), format, length = 2L, use_names = TRUE)
  res[2L, ]
}

#' @export
vec_restore.links <- function(x, to, ...) {
  validate_links(NextMethod())
}

#' @export
vec_ptype2.links.links <- function(x, y, ...) x

#' @export
vec_ptype2.list.links <- function(x, y, ...) y

#' @export
vec_ptype2.links.list <- function(x, y, ...) x

#' @export
vec_ptype2.data.frame.links <- function(x, y, ...) y

#' @export
vec_ptype2.links.data.frame <- function(x, y, ...) x

#' @export
vec_ptype2.link.links <- function(x, y, ...) y

#' @export
vec_ptype2.links.link <- function(x, y, ...) x

#' @export
vec_cast.links.links <- function(x, to, ...) x

#' @export
vec_cast.links.list <- function(x, to, ...) as_links(x)

#' @export
vec_cast.list.links <- function(x, to, ...) as.list(x)

#' @export
vec_cast.links.data.frame <- function(x, to, ...) as_links(x)

#' @export
vec_cast.data.frame.links <- function(x, to, ...) as.data.frame(x)

#' @export
vec_cast.links.link <- function(x, to, ...) as_links(x)

#' @export
c.links <- function(...) {

  args <- lapply(list(...), as_links)

  validate_links(
    do.call(vec_c, c(args, list(.ptype = args[[1L]])))
  )
}

#' @export
`[.links` <- function(x, i, ...) {

  res <- links_slice(x, vec_as_location(i, length(x), names(x)))

  if (is.null(res)) {
    return(links())
  }

  res
}

#' @export
`[<-.links` <- function(x, i, ..., value) {

  i <- vec_as_location(i, length(x), names(x))

  if (is.null(value)) {
    return(links_slice(x, -i))
  }

  value <- list_to_list_of_links(value)

  trg_ids <- field(links_slice(x, i), "id")

  if (is.null(names(value))) {
    names(value) <- trg_ids
  }

  new_ids <- names(value)

  if (!setequal(new_ids, trg_ids)) {
    stop(
      "Replacing IDs ", paste_enum(trg_ids), " with ", paste_enum(new_ids),
      " is not allowed."
    )
  }

  links_assign(x, i, value[trg_ids])
}

#' @export
`[[.links` <- function(x, i, ...) {

  res <- vec_proxy(
    links_slice(x, vec_as_location2(i, length(x), names(x)))
  )

  as_link(res[, colnames(res) != "id"])
}

#' @export
`[[<-.links` <- function(x, i, ..., value) {

  i <- vec_as_location2(i, length(x), names(x))
  val <- set_names(list(as_link(value)), x$id[i])

  links_assign(x, i, val)
}

#' @export
`$.links` <- function(x, name) {
  field(x, name)
}

#' @export
`$<-.links` <- function(x, name, value) {
  `field<-`(x, name, value)
}

#' @rdname new_link
#' @export
as_links <- function(x) {
  UseMethod("as_links")
}

#' @rdname new_link
#' @export
as_links.links <- function(x) {
  validate_links(x)
}

#' @rdname new_link
#' @export
as_links.link <- function(x) {
  as_links(as.list(x))
}

#' @rdname new_link
#' @export
as_links.list <- function(x) {
  do.call(links, x)
}

#' @rdname new_link
#' @export
as_links.NULL <- function(x) {
  links()
}

#' @method as_links data.frame
#' @rdname new_link
#' @export
as_links.data.frame <- function(x) {
  as_links(as.list(x))
}

#' @method as.data.frame links
#' @export
as.data.frame.links <- function(x, ...) {
  vec_proxy(x)
}

#' @method as.matrix links
#' @export
as.matrix.links <- function(x, ...) {
  todo <- x$from != "" & x$to != ""
  as_adjacency_matrix(x$from[todo], x$to[todo])
}

#' @rdname topo_sort
#' @export
is_acyclic.links <- function(x) {
  is_acyclic(as.matrix(x))
}

#' @method as.list links
#' @export
as.list.links <- function(x, ...) {

  df <- vec_proxy(x)

  lst <- split(df, seq_len(nrow(df)))

  res <- map(
    function(x, cols) as_link(lapply(x[, cols], na_to_empty)),
    lst,
    lapply(lapply(lst, colnames), setdiff, "id")
  )

  names(res) <- chr_xtr(lst, "id")

  res
}

links_slice <- function(...) {
  validate_links(vec_slice(...))
}

links_assign <- function(...) {
  validate_links(vec_assign(...))
}

#' @rdname new_link
#' @export
validate_links <- function(x) {

  any_dup <- function(x) {
    anyDuplicated(Filter(Negate(is.na), x))
  }

  if (is_board(x)) {
    x <- board_links(x)
  }

  if (!is_links(x)) {
    abort(
      "Expecting a board links objects to inherit from \"links\".",
      class = "links_class_invalid"
    )
  }

  for (y in as.list(x)) {
    validate_link(y)
  }

  fields <- c("id", "to", "input")

  if (!all(fields %in% fields(x))) {
    abort(
      paste0(
        "Expecting the links to contain at least fields ", paste_enum(fields)
      ),
      class = "links_fields_invalid"
    )
  }

  if (any_dup(field(x, "id")) != 0L) {
    abort(
      "Links IDs are required to be unique.",
      class = "links_names_unique_invalid"
    )
  }

  dup_inp <- lapply(split(field(x, "input"), field(x, "to")), any_dup)
  dup_inp <- lgl_ply(dup_inp, Negate(identical), 0L, use_names = TRUE)

  if (any(dup_inp)) {
    abort(
      paste0(
        "Block(s) ", paste_enum(names(dup_inp)[dup_inp]), " have ",
        "multiple identical inputs."
      ),
      class = "links_block_inputs_invalid"
    )
  }

  if (!is_acyclic(x)) {
    abort("Links form a cycle.", class = "links_acyclic_invalid")
  }

  x
}

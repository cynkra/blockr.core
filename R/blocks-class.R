#' @rdname new_block
#' @export
blocks <- function(...) {

  args <- list(...)

  args <- lapply(args, as_block)

  if (is.null(names(args))) {
    names(args) <- rand_names(n = length(args))
  }

  miss <- is.na(names(args)) | names(args) == ""

  if (any(miss)) {
    names(args)[miss] <- rand_names(names(args)[!miss], sum(miss))
  }

  validate_blocks(
    new_vctr(args, class = "blocks")
  )
}

#' @rdname new_block
#' @export
is_blocks <- function(x) {
  inherits(x, "blocks")
}

#' @rdname new_block
#' @export
as_blocks <- function(x, ...) {
  UseMethod("as_blocks")
}

#' @rdname new_block
#' @export
as_blocks.blocks <- function(x, ...) {
  validate_blocks(x)
}

#' @rdname new_block
#' @export
as_blocks.list <- function(x, ...) {
  do.call(blocks, x)
}

#' @rdname new_block
#' @export
as_blocks.block <- function(x, ...) {
  as_blocks(list(x))
}

#' @export
as.list.blocks <- function(x, ...) {
  vec_data(x)
}

#' @export
`names<-.blocks` <- function(x, value) {

  if (is.null(value)) {
    value <- rep("", length(x))
  } else if (anyDuplicated(value) != 0L) {
    stop("IDs are required to be unique.")
  }

  attr(x, "names") <- value

  x
}

#' @export
format.blocks <- function(x, ...) {

  out <- lapply(x, format)
  out <- Map(c, names(x), lapply(out, function(x) paste0(" ", x)))
  out <- lapply(out, c, "")
  out <- unlst(out)

  c(
    paste0("<", class(x)[1L], "[", length(x), "]>"),
    if (length(x)) "",
    out[-length(out)]
  )
}

#' @export
print.blocks <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
vec_restore.blocks <- function(x, to, ...) {
  validate_blocks(NextMethod())
}

#' @export
vec_ptype2.blocks.blocks <- function(x, y, ...) x

#' @export
vec_ptype2.list.blocks <- function(x, y, ...) y

#' @export
vec_ptype2.blocks.list <- function(x, y, ...) x

#' @export
vec_ptype2.block.blocks <- function(x, y, ...) y

#' @export
vec_ptype2.blocks.block <- function(x, y, ...) x

#' @export
vec_cast.blocks.blocks <- function(x, to, ...) x

#' @export
vec_cast.blocks.list <- function(x, to, ...) as_blocks(x)

#' @export
vec_cast.list.blocks <- function(x, to, ...) as.list(x)

#' @export
vec_cast.blocks.block <- function(x, to, ...) as_blocks(x)

#' @export
c.blocks <- function(...) {

  args <- lapply(list(...), as_blocks)
  args <- lapply(args, as.list)
  args <- do.call(c, args)

  validate_blocks(
    do.call(c, args)
  )
}

#' @export
`[<-.blocks` <- function(x, i, ..., value) {

  i <- vec_as_location(i, length(x), names(x))

  if (is.null(value)) {
    return(blocks_slice(x, -i))
  }

  value <- as_blocks(value)

  trg_ids <- names(x)[i]

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

  blocks_assign(x, i, value[trg_ids])
}

blocks_slice <- function(...) {
  validate_blocks(vec_slice(...))
}

blocks_assign <- function(...) {
  validate_blocks(vec_assign(...))
}

validate_blocks <- function(x) {

  if (!is_blocks(x)) {
    stop("Expecting blocks to inherit from \"blocks\".")
  }

  if (!is.list(x) || !all(lgl_ply(x, is_block))) {
    stop("Expecting the board to contain a set of blocks.")
  }

  ids <- names(x)

  if (length(ids) != length(x) || any(is.na(ids) | !nchar(ids))) {
    stop("Block IDs are required to be nonempty strings.")
  }

  if (anyDuplicated(ids) != 0) {
    stop("Block IDs are required to be unique.")
  }

  x
}

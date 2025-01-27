#' Board
#'
#' Blocks are placed on a board.
#'
#' @param blocks List of blocks
#' @param links Data frame with columns `from` and `to`
#' @param ... Further (metadata) attributes
#' @param class Board sub-class
#'
#' @export
new_board <- function(blocks = list(), links = NULL, ..., class = character()) {

  blocks <- as_blocks(blocks)
  links <- as_links(links)

  links <- complete_unary_inputs(links, blocks)
  links <- complete_variadic_inputs(links, blocks)

  validate_board(
    structure(
      list(blocks = blocks, links = links, ...),
      class = c(class, "board")
    )
  )
}

complete_unary_inputs <- function(x, blocks) {

  ids <- names(blocks)

  to_complete <- x$to %in% ids[int_ply(blocks, block_arity) == 1L] & (
    is.na(x$input) | nchar(x$input) == 0L
  )

  inputs <- lapply(blocks[unique(x$to[to_complete])], block_inputs)

  x$input[to_complete] <- chr_ply(inputs[x$to[to_complete]], identity)

  x
}

complete_variadic_inputs <- function(x, blocks) {

  ids <- names(blocks)

  to_complete <- x$to %in% ids[is.na(int_ply(blocks, block_arity))] & (
    is.na(x$input) | nchar(x$input) == 0L
  )

  to_todo <- x$to[to_complete]

  x$input[to_complete] <- ave(to_todo, to_todo, FUN = seq_along)

  x
}

validate_board_blocks_links <- function(blocks, links) {

  ids <- names(blocks)

  if (!all(links$from %in% ids) || !all(links$to %in% ids)) {
    stop("Expecting all links to refer to known block IDs.")
  }

  arity <- int_ply(blocks, block_arity)

  for (i in Filter(Negate(is.na), unique(arity))) {

    fail <- table(factor(links$to, levels = ids[arity == i])) > i

    if (any(fail)) {
      stop(
        i, "-ary blocks are expected to have at most ", i, " incoming ",
        "edge(s), which does not hold for block(s) ",
        paste_enum(names(fail)[fail]), "."
      )
    }
  }

  inputs <- lapply(blocks[!is.na(arity)], block_inputs)

  for (i in intersect(links$to, names(inputs))) {

    unknown <- setdiff(links$input[links$to == i], inputs[[i]])

    if (length(unknown)) {
      stop(
        "Block ", i, " expects inputs ", paste_enum(inputs[[i]]),
        " but received ", paste_enum(unknown)
      )
    }
  }

  invisible()
}

#' @param x Board object
#' @rdname new_board
#' @export
validate_board <- function(x) {

  if (!is_board(x)) {
    stop("Expecting a board object to inherit from \"baord\".")
  }

  if (!is.list(x)) {
    stop("Expecting a board object to be list-like.")
  }

  cmps <- c("blocks", "links")

  if (!all(cmps %in% names(x))) {
    stop(
      "Expecting a board object to contain components ", paste_enum(cmps), "."
    )
  }

  validate_board_blocks_links(
    board_blocks(x),
    board_links(x)
  )

  x
}

#' @rdname new_board
#' @export
is_board <- function(x) {
  inherits(x, "board")
}

#' @export
sort.board <- function(x, decreasing = FALSE, ...) {

  res <- topo_sort(as.matrix(x))
  ids <- board_block_ids(x)

  ind <- match(res, ids)

  if (isTRUE(decreasing)) {
    ind <- rev(ind)
  }

  board_blocks(x)[ind]
}

#' @export
as.matrix.board <- function(x, ...) {

  block_ids <- board_block_ids(x)
  links <- board_links(x)

  as_adjacency_matrix(links$from, links$to, block_ids)
}

#' @rdname topo_sort
#' @export
is_acyclic.board <- function(x) {
  is_acyclic(as.matrix(x))
}

#' @rdname serve
#' @export
serve.board <- function(x, ...) {

  id <- rand_names()

  ui <- bslib::page_fluid(
    board_ui(id, x,
      list(
        preseve_board = ser_deser_ui,
        manage_blocks = add_rm_block_ui,
        manage_links = add_rm_link_ui
      )
    )
  )

  server <- function(input, output, session) {
    board_server(id, x,
      list(
        preseve_board = ser_deser_server,
        manage_blocks = add_rm_block_server,
        manage_links = add_rm_link_server,
        notify_user = block_notification_server
      )
    )
  }

  shinyApp(ui, server)
}

#' @rdname new_board
#' @export
board_blocks <- function(x) {
  stopifnot(is_board(x))
  validate_blocks(x[["blocks"]])
}

#' @param value Replacement value
#' @rdname new_board
#' @export
`board_blocks<-` <- function(x, value) {
  stopifnot(is_board(x))
  x[["blocks"]] <- value
  validate_board(x)
}

#' @rdname new_board
#' @export
board_links <- function(x) {
  stopifnot(is_board(x))
  validate_links(x[["links"]])
}

#' @rdname new_board
#' @export
`board_links<-` <- function(x, value) {
  stopifnot(is_board(x))
  x[["links"]] <- value
  validate_board(x)
}

#' @rdname new_board
#' @export
board_block_ids <- function(x) {
  names(board_blocks(x))
}

#' @rdname new_board
#' @export
board_link_ids <- function(x) {
  names(board_links(x))
}

#' @param add Links to add
#' @param rm Link IDs to remove
#' @rdname new_board
#' @export
modify_links <- function(x, add = NULL, rm = NULL) {

  links <- board_links(x)

  if (is_link(rm)) {
    rm <- names(rm)
  }

  if (length(rm)) {
    stopifnot(is.character(rm), all(rm %in% names(links)))
    links <- links[!names(links) %in% rm, ]
  }

  board_links(x) <- c(links, add)

  x
}

#' @rdname new_block
#' @export
block_inputs.board <- function(x, ...) {
  lapply(set_names(board_blocks(x), board_block_ids(x)), block_inputs)
}

#' @export
format.board <- function(x, ...) {

  out <- ""

  for (cl in rev(class(x))) {
    out <- paste0("<", cl, out, ">")
  }

  blk <- board_blocks(x)

  if (length(blk)) {

    blk <- lapply(blk, format, ...)
    blk <- lapply(blk, c, "")

    out <- c(
      out,
      "",
      paste0("Blocks[", length(blk), "]:"),
      "",
      unlst(blk)
    )
  }

  lnk <- board_links(x)

  if (length(lnk)) {

    out <- c(
      out,
      paste0("Links[", length(lnk), "]:"),
      "",
      paste0(names(lnk), ": ", format(lnk))
    )
  }

  out
}

#' @export
print.board <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' Board
#'
#' Blocks are placed on a board.
#'
#' @param blocks List of blocks
#' @param links Data frame with columns `from` and `to`
#' @param ... Further (metadata) attributes
#' @param id Board ID
#' @param class Board sub-class
#'
#' @export
new_board <- function(blocks = list(), links = NULL, ...,
                      id = "board", class = character()) {

  blocks <- as_blocks(blocks)
  links <- as_links(links)

  ids <- names(blocks)

  to_complete <- links$to %in% ids[int_ply(blocks, block_arity) == 1L] & (
    is.na(links$input) | nchar(links$input) == 0L
  )

  inputs <- set_names(lapply(blocks, block_inputs), ids)

  links$input[to_complete] <- chr_ply(inputs[links$to[to_complete]], identity)

  validate_board_blocks_links(blocks, links)

  structure(
    list(blocks = blocks, links = links, ...),
    id = id,
    class = c(class, "board")
  )
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

  inputs <- set_names(lapply(blocks, block_inputs), ids)

  for (i in unique(links$to)) {

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

#' @rdname serve
#' @export
serve.board <- function(x, ...) {

  id <- rand_names()

  ui <- bslib::page_fluid(
    board_ui(
      x,
      id,
      ser_deser = ser_deser_ui,
      add_rm_block = add_rm_block_ui,
      add_rm_link = add_rm_link_ui
    )
  )

  server <- function(input, output, session) {
    board_server(
      x,
      id,
      ser_deser = ser_deser_server,
      add_rm_block = add_rm_block_server,
      add_rm_link = add_rm_link_server,
      block_notifications = block_notification_server
    )
  }

  shinyApp(ui, server)
}

#' @rdname new_board
#' @export
board_id <- function(x) {
  stopifnot(is_board(x))
  attr(x, "id")
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
block_inputs.board <- function(x) {
  lapply(set_names(board_blocks(x), board_block_ids(x)), block_inputs)
}

#' @export
format.board <- function(x, ...) {

  out <- ""

  for (cl in rev(class(x))) {
    out <- paste0("<", cl, out, ">")
  }

  blk <- board_blocks(x)

  out <- c(
    paste0(board_id(x), out),
    "",
    paste0("Blocks[", length(blk), "]:"),
    ""
  )

  blk <- lapply(blk, format, ...)
  blk <- lapply(blk, c, "")

  lnk <- board_links(x)

  out <- c(
    out,
    unlst(blk),
    paste0("Links[", length(lnk), "]:"),
    "",
    paste0(names(lnk), ": ", format(lnk))
  )
}

#' @export
print.board <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

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
new_board <- function(blocks = list(), links = data.frame(from = character(),
                                                          to = character(),
                                                          input = character()),
                      ..., id = "board", class = character()) {

  if (is_block(blocks)) {
    blocks <- list(blocks)
  }

  validate_board(
    structure(
      list(blocks = blocks, links = links, ...),
      id = id,
      class = c(class, "board")
    )
  )
}

validate_board <- function(x) {

  blocks <- board_blocks(x)
  links <- board_links(x)

  if (!is.list(blocks) || !all(lgl_ply(blocks, is_block))) {
    browser()
    stop("Expecting the board to contain a set of blocks.")
  }

  if (!is.data.frame(links)) {
    stop("Expecting links to be represented by a data.frame.")
  }

  link_cols <- c("from", "to", "input")

  if (!setequal(colnames(links), link_cols)) {
    stop(
      "Expecting the link data.frame to contain columns ",
      paste_enum(link_cols)
    )
  }

  ids <- chr_ply(blocks, block_uid)

  if (anyDuplicated(ids) != 0) {
    stop("Block IDs are required to be unique.")
  }

  if (!all(links$from %in% ids) || !all(links$to %in% ids)) {
    stop("Expecting all links to refer to known block IDs.")
  }

  if (any(links$from == links$to)) {
    stop("Self-referencing blocks are not allowed.")
  }

  arity <- int_ply(blocks, block_arity)

  for (i in Filter(Negate(is.na), unique(arity))) {

    fail <- table(factor(links$to, levels = ids[arity == i])) > i

    if (any(fail)) {
      stop(
        i, "-ary blocks are expected to have at most ", i, " incoming edges, ",
        "which does not hold for blocks ", paste_enum(names(fail)[fail]), "."
      )
    }
  }

  to_complete <- links$to %in% ids[arity == 1L] & (
    is.na(links$input) | nchar(links$input) == 0L
  )

  inputs <- set_names(lapply(blocks, block_inputs), ids)

  links$input[to_complete] <- inputs[links$to[to_complete]]

  for (i in unique(links$to)) {

    actual <- links$input[links$to == i]

    if (!setequal(actual, inputs[[i]])) {
      stop(
        "Block ", i, " expects inputs ", paste_enum(inputs[[i]]),
        " but received ", paste_enum(actual)
      )
    }
  }

  x
}

#' @param x A board object
#' @rdname new_board
#' @export
is_board <- function(x) {
  inherits(x, "board")
}

#' @export
sort.board <- function(x, decreasing = FALSE, ...) {

  blk <- x[["blocks"]]
  ids <- chr_ply(blk, block_uid)

  res <- topo_sort(ids, x[["links"]])

  blk[match(res, ids)]
}

#' @rdname serve
#' @export
serve.board <- function(x, ...) {

  ui <- bslib::page_fluid(board_ui(x))

  server <- function(input, output, session) {
    board_server(x)
  }

  shinyApp(ui, server)
}

board_id <- function(x) {
  stopifnot(is_board(x))
  attr(x, "id")
}

board_blocks <- function(x) {
  stopifnot(is_board(x))
  x[["blocks"]]
}

`board_blocks<-` <- function(x, value) {
  stopifnot(is_board(x))
  x[["blocks"]] <- value
  validate_board(x)
}

board_links <- function(x) {
  stopifnot(is_board(x))
  x[["links"]]
}

`board_links<-` <- function(x, value) {
  stopifnot(is_board(x))
  x[["links"]] <- value
  validate_board(x)
}

block_ids <- function(x) {
  chr_ply(board_blocks(x), block_uid)
}

add_block <- function(x, blk) {

  stopifnot(is_board(x), is_block(blk))

  board_blocks(x) <- c(board_blocks(x), list(blk))

  x
}

remove_blocks <- function(x, ids) {

  stopifnot(is_board(x), is.character(ids), all(ids %in% block_ids(x)))

  links <- board_links(x)

  board_links(x) <- links[!links$from %in% ids & !links$to %in% ids, ]
  board_blocks(x) <- board_blocks(x)[!block_ids(x) %in% ids]

  x
}

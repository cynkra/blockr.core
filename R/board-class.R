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

  stopifnot(
    all(lgl_ply(blocks, is_block)),
    is.data.frame(links), setequal(colnames(links), c("from", "to", "input"))
  )

  ids <- chr_ply(blocks, block_uid)

  stopifnot(
    all(links$from %in% ids), all(links$to %in% ids),
    anyDuplicated(ids) == 0, !any(links$from == links$to)
  )

  arity <- int_ply(blocks, block_arity)

  for (i in Filter(Negate(is.na), unique(arity))) {

    fail <- table(factor(links$to, levels = ids[arity == i])) != i

    if (any(fail)) {
      stop(
        i, "-ary blocks are expected to have exactly ", i, " incoming edges, ",
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

  structure(
    list(blocks = blocks, links = links, ...),
    id = id,
    class = c(class, "board")
  )
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

add_block <- function(x, blk) {

  stopifnot(is_board(x), is_block(blk))

  x[["blocks"]] <- c(x[["blocks"]], blk)

  x
}

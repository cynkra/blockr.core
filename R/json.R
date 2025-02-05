#' Serialization utilities
#'
#' Object (de)serialization.
#'
#' @param x Object to (de)serialize
#' @param ... Generic consistency
#'
#' @export
blockr_ser <- function(x, ...) {
  UseMethod("blockr_ser")
}

#' @param state Object state (as returned from an `expr_server`)
#' @rdname blockr_ser
#' @export
blockr_ser.block <- function(x, state = NULL, ...) {
  as.list(x, state)
}

#' @param blocks Block states (`NULL` defaults to values from ctor scope)
#' @rdname blockr_ser
#' @export
blockr_ser.blocks <- function(x, blocks = NULL, ...) {

  if (!length(blocks)) {

    res <- lapply(x, blockr_ser)

  } else {

    stopifnot(
      is.list(blocks), all(lgl_ply(blocks, is.list)),
      length(blocks) == length(x), setequal(names(blocks), names(x))
    )

    res <- Map(blockr_ser, x, blocks[names(x)])
  }

  list(
    object = class(x),
    payload = res
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.board <- function(x, blocks = NULL, ...) {
  list(
    object = class(x),
    blocks = blockr_ser(board_blocks(x), blocks),
    links = lapply(board_links(x), blockr_ser),
    version = as.character(pkg_version())
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.link <- function(x, ...) {
  list(
    object = class(x),
    payload = as.list(x)
  )
}

#' @rdname blockr_ser
#' @export
blockr_ser.links <- function(x, ...) {
  list(
    object = class(x),
    payload = lapply(x, blockr_ser)
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser <- function(x, ...) {
  UseMethod("blockr_deser")
}

#' @rdname blockr_ser
#' @export
blockr_deser.list <- function(x, ...) {
  stopifnot("object" %in% names(x))
  blockr_deser(structure(list(), class = x[["object"]]), data = x)
}

#' @param data List valued data (converted from JSON)
#' @rdname blockr_ser
#' @export
blockr_deser.block <- function(x, data, ...) {
  as_block(data)
}

#' @rdname blockr_ser
#' @export
blockr_deser.blocks <- function(x, data, ...) {
  as_blocks(
    lapply(data[["payload"]], blockr_deser)
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser.board <- function(x, data, ...) {
  new_board(
    blockr_deser(data[["blocks"]]),
    lapply(data[["links"]], blockr_deser),
    class = setdiff(class(x), "board")
  )
}

#' @rdname blockr_ser
#' @export
blockr_deser.link <- function(x, data, ...) {
  as_link(data[["payload"]])
}

#' @rdname blockr_ser
#' @export
blockr_deser.links <- function(x, data, ...) {
  as_links(
    lapply(data[["payload"]], blockr_deser)
  )
}

#' @rdname blockr_ser
#' @export
to_json <- function(x, ...) {
  jsonlite::toJSON(blockr_ser(x, ...), auto_unbox = TRUE)
}

#' @rdname blockr_ser
#' @export
from_json <- function(x) {

  if (is_string(x) && file.exists(x)) {
    x <- readLines(x)
  } else if (is.character(x) && length(x) > 1L) {
    x <- paste0(x, collapse = "")
  }

  blockr_deser(
    jsonlite::fromJSON(x, simplifyDataFrame = FALSE)
  )
}

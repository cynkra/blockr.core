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

  pkg <- attr(x, "ctor_pkg")

  if (is.null(state)) {
    state <- initial_block_state(x)
  }

  list(
    object = class(x),
    payload = state,
    constructor = attr(x, "ctor"),
    package = pkg,
    version = as.character(utils::packageVersion(pkg))
  )
}

#' @param blocks Block states (`NULL` defaults to values from ctor scope)
#' @rdname blockr_ser
#' @export
blockr_ser.board <- function(x, blocks = NULL, ...) {

  blks <- board_blocks(x)

  if (is.null(blocks)) {

    blks <- lapply(blks, blockr_ser)

  } else {

    stopifnot(
      length(blocks) == length(blks), setequal(names(blocks), names(blks))
    )

    blks <- Map(blockr_ser, blks, blocks[names(blks)])
  }

  list(
    object = class(x),
    blocks = blks,
    links = lapply(board_links(x), blockr_ser),
    id = attr(x, "id"),
    version = as.character(utils::packageVersion(utils::packageName()))
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

  stopifnot(
    all(c("constructor", "payload", "package") %in% names(data))
  )

  ctor <- get(
    data[["constructor"]],
    asNamespace(data[["package"]]),
    mode = "function"
  )

  args <- setdiff(names(formals(ctor)), "...")

  args <- c(
    data[["payload"]][args],
    ctor = data[["constructor"]],
    ctor_pkg = data[["package"]],
    uid = data[["uid"]]
  )

  do.call(ctor, args)
}

#' @rdname blockr_ser
#' @export
blockr_deser.board <- function(x, data, ...) {
  new_board(
    lapply(data[["blocks"]], blockr_deser),
    lapply(data[["links"]], blockr_deser),
    id = data[["id"]],
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

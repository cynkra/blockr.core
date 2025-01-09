#' Serialization
#'
#' Object (de)serialization.
#'
#' @param x Object to (de)serialize
#' @param state Generic consistency
#'
#' @export
to_json <- function(x, ...) {
  UseMethod("to_json")
}

#' @param state Object state (as returned from an `expr_server`)
#' @rdname to_json
#' @export
to_json.block <- function(x, state, ...) {

  pkg <- attr(x, "ctor_pkg")

  res <- list(
    object = class(x),
    uid = block_uid(x),
    payload = state,
    constructor = attr(x, "ctor"),
    package = pkg,
    version = as.character(utils::packageVersion(pkg))
  )

  jsonlite::toJSON(res, auto_unbox = TRUE)
}

#' @param blocks JSON-serialized blocks
#' @rdname to_json
#' @export
to_json.board <- function(x, blocks, ...) {

  stopifnot(all(lgl_ply(blocks, inherits, "json")))

  paste0(
    "{",
      "\"object\":[", paste0("\"", class(x), "\"", collapse = ", "), "],",
      "\"blocks\":[", paste0(blocks, collapse = ", "), "],",
      "\"links\":", jsonlite::toJSON(x[["links"]], auto_unbox = TRUE), ",",
      "\"version\":\"",
        as.character(utils::packageVersion(utils::packageName())),
      "\"",
    "}"
  )
}

#' @rdname to_json
#' @export
from_json <- function(x, ...) {
  UseMethod("from_json")
}

#' @rdname to_json
#' @export
from_json.json <- function(x, ...) {
  from_json(as.character(x), ...)
}

#' @rdname to_json
#' @export
from_json.character <- function(x, ...) {

  dat <- jsonlite::fromJSON(x)

  stopifnot("object" %in% names(dat))

  from_json(structure(list(), class = dat[["object"]]), dat)
}

#' @rdname to_json
#' @export
from_json.block <- function(x, data, ...) {

  stopifnot(
    all(c("constructor", "payload", "package", "uid") %in% names(data))
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

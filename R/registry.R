block_registry <- new.env()

#' Block registry
#'
#' Register, list and retrieve available blocks.
#'
#' @param constructor Block constructor
#' @param name,description Metadata describing the block
#' @param classes Block classes
#' @param uid Unique ID for a registry entry
#' @param category Useful to sort blocks by topics. If not specified,
#'   blocks are uncategorized.
#' @param overwrite Overwrite existing entry
#'
#' @export
register_block <- function(constructor, name, description,
                           classes = class(constructor()),
                           uid = classes[1L],
                           category = "uncategorized",
                           overwrite = FALSE) {

  if (uid %in% list_blocks() && !isTRUE(overwrite)) {
    stop("block ", uid, " already exists. Try removing or `overwrite = FALSE`")
  }

  entry <- new_registry_entry(
    constructor,
    name = name,
    description = description,
    classes = classes,
    category = category
  )

  assign(uid, entry, envir = block_registry)
}

new_registry_entry <- function(constructor, ...) {
  structure(constructor, ..., class = "registry_entry")
}

is_registry_entry <- function(x) inherits(x, "registry_entry")

list_blocks <- function() {
  ls(envir = block_registry)
}

#' @rdname register_block
#' @export
unregister_block <- function(uid) {
  rm(uid, envir = block_registry, inherits = FALSE)
}

#' @param ... Forwarded to `register_block()`
#' @rdname register_block
#' @export
register_blocks <- function(...) {

  arg_processor <- function(constructor, ...) {

    wrap_list <- function(x) {
      if (length(x) > 1L) list(x) else x
    }

    if (length(constructor) > 1L) {
      return(c(list(constructor), list(...)))
    }

    c(list(constructor), lapply(list(...), wrap_list))
  }

  invisible(
    do.call(Map, c(register_block, arg_processor(...)))
  )
}

get_registry_entry <- function(uid) {
  res <- get(uid, envir = block_registry, inherits = FALSE)
  stopifnot(is_registry_entry(res))
  res
}

#' @rdname register_block
#' @export
available_blocks <- function() {
  lapply(set_names(nm = list_blocks()), get_registry_entry)
}

register_core_blocks <- function() {
  register_blocks(
    constructor = c(
      new_dataset_block
    ),
    name = c(
      "dataset block"
    ),
    description = c(
      "Choose a dataset from a package"
    ),
    category = c(
      "data"
    )
  )
}

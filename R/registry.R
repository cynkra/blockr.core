block_registry <- new.env()

#' Block registry
#'
#' Register, list and retrieve available blocks.
#'
#' @param ctor Block constructor
#' @param name,description Metadata describing the block
#' @param classes Block classes
#' @param uid Unique ID for a registry entry
#' @param category Useful to sort blocks by topics. If not specified,
#'   blocks are uncategorized.
#' @param package Package where constructor is defined (or `NULL`)
#' @param overwrite Overwrite existing entry
#'
#' @export
register_block <- function(ctor, name, description, classes = NULL, uid = NULL,
                           category = "uncategorized", package = NULL,
                           overwrite = FALSE) {

  if (is.function(ctor)) {
    package <- utils::packageName(environment(ctor))
  }

  ctor_name <- NULL

  if (is_string(ctor)) {
    stopifnot(is_string(package))
    ctor_name <- ctor
    ctor <- get(ctor, asNamespace(package), mode = "function")
  }

  if (is.null(classes)) {
    stopifnot(is_string(ctor_name), is_string(package))
    obj <- ctor(ctor = ctor_name, ctor_pkg = package)
    classes <- class(obj)
  }

  if (is.null(uid)) {
    uid <- classes[1L]
  }

  if (uid %in% list_blocks() && !isTRUE(overwrite)) {
    stop("block ", uid, " already exists. Try removing or `overwrite = FALSE`")
  }

  entry <- new_registry_entry(
    ctor,
    name = name,
    description = description,
    classes = classes,
    category = category
  )

  assign(uid, entry, envir = block_registry)
}

new_registry_entry <- function(ctor, ...) {
  structure(ctor, ..., class = "registry_entry")
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

  arg_processor <- function(ctor, ...) {

    wrap_list <- function(x) {
      if (length(x) > 1L) list(x) else x
    }

    if (length(ctor) > 1L) {
      return(c(list(ctor), list(...)))
    }

    c(list(ctor), lapply(list(...), wrap_list))
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
    c(
      "new_dataset_block",
      "new_subset_block"
    ),
    name = c(
      "dataset block",
      "subset block"
    ),
    description = c(
      "Choose a dataset from a package",
      "Row and column subsetting"
    ),
    category = c(
      "data",
      "transform"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}

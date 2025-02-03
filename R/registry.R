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
    category = category,
    ctor_name = ctor_name,
    package = package
  )

  assign(uid, entry, envir = block_registry)
}

new_registry_entry <- function(ctor, ...) {
  structure(ctor, ..., class = "registry_entry")
}

is_registry_entry <- function(x) inherits(x, "registry_entry")

#' @rdname register_block
#' @export
list_blocks <- function() {
  ls(envir = block_registry)
}

#' @rdname register_block
#' @export
unregister_blocks <- function(uid = list_blocks()) {
  rm(list = uid, envir = block_registry, inherits = FALSE)
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

#' @param id Block ID as reported by `list_blocks()`
#' @rdname register_block
#' @export
create_block <- function(id, ...) {
  ctor <- get_registry_entry(id)
  ctor(..., ctor = attr(ctor, "ctor_name"), ctor_pkg = attr(ctor, "package"))
}

register_core_blocks <- function(which = get_option("blocks", "all")) {

  blocks <- paste0(
    c("dataset", "subset", "merge", "rbind", "scatter", "upload", "csv",
      "static", "head"),
    "_block"
  )

  if (identical(which, "all")) {
    which <- blocks
  } else {
    stopifnot(all(which %in% blocks), anyDuplicated(which) == 0L)
  }

  blocks <- match(which, blocks)

  register_blocks(
    c(
      "new_dataset_block",
      "new_subset_block",
      "new_merge_block",
      "new_rbind_block",
      "new_scatter_block",
      "new_upload_block",
      "new_csv_block",
      "new_static_block",
      "new_head_block"
    )[blocks],
    name = c(
      "dataset block",
      "subset block",
      "merge block",
      "rbind block",
      "scatter plot block",
      "data upload block",
      "csv parser block",
      "static data block",
      "head/tail block"
    )[blocks],
    description = c(
      "Choose a dataset from a package",
      "Row and column subsetting",
      "Joining or datasets",
      "Row-binding of datasets",
      "Scatter plotting",
      "Upload data",
      "Read CSV file",
      "Static data",
      "Data head/tail"
    )[blocks],
    category = c(
      "data",
      "transform",
      "transform",
      "transform",
      "plot",
      "file",
      "parse",
      "data",
      "transform"
    )[blocks],
    package = utils::packageName(),
    overwrite = TRUE
  )
}

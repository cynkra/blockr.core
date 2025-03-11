#' Board plugin
#'
#' Plugins are used to customize/enhance UX aspects of the board bodule.
#'
#' @param server,ui Server/UI for the plugin module
#' @param validator Validator function that validates server return values
#' @param class Plugin subclass
#'
#' @export
new_plugin <- function(server, ui = NULL, validator = function(x, ...) x,
                       class = character()) {

  res <- structure(
    list(server = server, ui = ui),
    validator = validator,
    class = c(class, "plugin")
  )

  validate_plugin(res)
}

#' @param x Plugin object
#' @rdname new_plugin
#' @export
is_plugin <- function(x) {
  inherits(x, "plugin") &&
    sum(inherits(x, known_plugins(), which = TRUE) != 0L) == 1L
}

known_plugins <- function() {
  c("preserve_board", "manage_blocks", "manage_links", "manage_stacks",
    "notify_user", "generate_code", "edit_block", "edit_stack")
}

#' @rdname new_plugin
#' @export
as_plugin <- function(x) {
  UseMethod("as_plugin")
}

#' @rdname new_plugin
#' @export
as_plugin.plugin <- function(x) x

#' @rdname new_plugin
#' @export
as_plugin.list <- function(x) do.call(new_plugin, x)

#' @rdname new_plugin
#' @export
as_plugin.plugins <- function(x) {

  if (length(x) != 1L) {
    abort(
      "Cannot cast length != 1 \"plugins\" to \"plugin\".",
      class = "plugins_as_plugin_invalid"
    )
  }

  .subset2(x, 1L)
}

#' @export
as.list.plugin <- function(x, ...) {
  list(
    server = plugin_server(x),
    ui = plugin_ui(x),
    validator = plugin_validator(x),
    class = setdiff(class(x), "plugin")
  )
}

plugin_server <- function(x) x[["server"]]

plugin_ui <- function(x) x[["ui"]]

plugin_validator <- function(x) attr(x, "validator")

plugin_id <- function(x) class(x)[1L]

validate_plugin <- function(x) {

  if (!is_plugin(x)) {
    abort(
      paste0(
        "Expecting a plugin to inherit from `plugin` and one of ",
        paste_enum(known_plugins()), "."
      ),
      class = "plugin_inheritance_invalid"
    )
  }

  if (!is.list(x)) {
    abort(
      "Expecting a plugin to behave list-like.",
      class = "plugin_type_invalid"
    )
  }

  if (!setequal(names(x), c("server", "ui"))) {
    abort(
      "Expecting a plugin to contain components \"server\" and \"ui\".",
      class = "plugin_components_invalid"
    )
  }

  srv <- plugin_server(x)

  if (!is.null(srv) && !is.function(srv)) {
    abort(
      "Expecting a plugin server to be a function.",
      class = "plugin_server_invalid"
    )
  }

  ui <- plugin_ui(x)

  if (!is.null(ui) && !is.function(ui)) {
    abort(
      "Expecting a plugin ui to be `NULL` or a function.",
      class = "plugin_ui_invalid"
    )
  }

  if (!is.function(plugin_validator(x))) {
    abort(
      "Expecting a plugin validator to be a function.",
      class = "plugin_validator_invalid"
    )
  }

  x
}

#' @rdname new_plugin
#' @export
preserve_board <- function(server, ui) {
  new_plugin(server, ui, validator = check_ser_deser_val,
             class = "preserve_board")
}

#' @rdname new_plugin
#' @export
manage_blocks <- function(server, ui) {
  new_plugin(server, ui, validator = expect_null, class = "manage_blocks")
}

#' @rdname new_plugin
#' @export
manage_links <- function(server, ui) {
  new_plugin(server, ui, validator = expect_null, class = "manage_links")
}

#' @rdname new_plugin
#' @export
manage_stacks <- function(server, ui) {
  new_plugin(server, ui, validator = expect_null, class = "manage_stacks")
}

#' @rdname new_plugin
#' @export
notify_user <- function(server, ui = NULL) {
  new_plugin(server, ui, validator = check_block_notifications_val,
             class = "notify_user")
}

#' @rdname new_plugin
#' @export
generate_code <- function(server, ui) {
  new_plugin(server, ui, validator = check_gen_code_val,
             class = "generate_code")
}

#' @rdname new_plugin
#' @export
edit_block <- function(server, ui) {
  new_plugin(server, ui, validator = expect_null, class = "edit_block")
}

#' @rdname new_plugin
#' @export
edit_stack <- function(server, ui) {
  new_plugin(server, ui, validator = expect_null, class = "edit_stack")
}

#' @export
format.plugin <- function(x, ...) {

  out <- ""

  for (cl in rev(class(x))) {
    out <- paste0("<", cl, out, ">")
  }

  out
}

#' @export
print.plugin <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

harmonize_list_of_plugins <- function(x) {
  if (is_plugin(x)) {
    list(x)
  } else if (is_plugins(x)) {
    as.list(x)
  } else if (is.list(x) && all(lgl_ply(x, is_plugin))) {
    x
  } else {
    list(as_plugin(x))
  }
}

#' @export
c.plugin <- function(...) {

  res <- unlist(
    lapply(list(...), harmonize_list_of_plugins),
    recursive = FALSE
  )

  as_plugins(res)
}

get_plugin <- function(plugin, plugins = NULL, component = NULL) {

  if (is.null(plugins)) {

    if (is.null(plugin)) {
      return(NULL)
    }

    res <- validate_plugin(plugin)

  } else {

    stopifnot(is_string(plugin), is_plugins(plugins))

    if (!plugin %in% chr_ply(plugins, plugin_id)) {
      return(NULL)
    }

    res <- plugins[[plugin]]
  }

  if (is.null(component)) {
    return(res)
  }

  switch(
    match.arg(component, c("server", "ui")),
    ui = plugin_ui(res),
    server = function(server_args, validator_args = NULL) {
      do.call(
        plugin_validator(res),
        c(
          list(do.call(plugin_server(res), server_args)),
          validator_args
        )
      )
    }
  )
}

get_plugin_ui <- function(plugin, plugins = NULL) {
  get_plugin(plugin, plugins, component = "ui")
}

get_plugin_server <- function(plugin, plugins = NULL) {
  get_plugin(plugin, plugins, component = "server")
}

call_plugin_server <- function(plugin, server_args, plugins = NULL,
                               validator_args = NULL) {

  server <- get_plugin_server(plugin, plugins)

  if (is.null(server)) {
    return(NULL)
  }

  if (is_plugin(plugin)) {
    id <- class(plugin)[1L]
  } else {
    id <- plugin
  }

  server(c(list(id), server_args), validator_args)
}

call_plugin_ui <- function(plugin, ns, ..., plugins = NULL) {

  ui <- get_plugin_ui(plugin, plugins)

  if (is.null(ui)) {
    return(tagList())
  }

  if (is_plugin(plugin)) {
    id <- class(plugin)[1L]
  } else {
    id <- plugin
  }

  ui(ns(id), ...)
}

#' @param which (Optional) character vectors of plugins to include
#' @rdname serve
#' @export
borad_plugins <- function(which = NULL) {

  plugins <- plugins(
    preserve_board(server = ser_deser_server, ui = ser_deser_ui),
    manage_blocks(server = add_rm_block_server, ui = add_rm_block_ui),
    manage_links(server = add_rm_link_server, ui = add_rm_link_ui),
    manage_stacks(server = add_rm_stack_server, ui = add_rm_stack_ui),
    notify_user(server = block_notification_server),
    generate_code(server = gen_code_server, ui = gen_code_ui),
    edit_block(server = edit_block_server, ui = edit_block_ui),
    edit_stack(server = edit_stack_server, ui = edit_stack_ui)
  )

  if (is.null(which)) {
    return(plugins)
  }

  plugins[which]
}

validate_callbacks <- function(x) {

  if (!is.list(x)) {
    stop("Expecting a list of callbacks.")
  }

  for (f in x) {
    if (!is.function(f)) {
      stop("Expecting callbacks to be passed as functions.")
    }
  }

  invisible()
}

#' @param ... Plugin objects
#' @rdname new_plugin
#' @export
plugins <- function(...) {
  validate_plugins(
    new_vctr(unname(list(...)), class = c("plugins", "list"))
  )
}

#' @rdname new_plugin
#' @export
is_plugins <- function(x) {
  inherits(x, "plugins")
}

#' @rdname new_plugin
#' @export
as_plugins <- function(x) {
  UseMethod("as_plugins")
}

#' @rdname new_plugin
#' @export
as_plugins.plugins <- function(x) {
  validate_plugins(x)
}

#' @rdname new_plugin
#' @export
as_plugins.list <- function(x) {
  do.call(plugins, x)
}

#' @rdname new_plugin
#' @export
as_plugins.plugin <- function(x) {
  as_plugins(list(x))
}

#' @export
as.list.plugins <- function(x, ...) {
  vec_data(x)
}

#' @export
anyDuplicated.plugins <- function(x, incomparables = FALSE, ...) {
  anyDuplicated(chr_ply(x, plugin_id), incomparables = incomparables, ...)
}

#' @rdname new_plugin
#' @export
validate_plugins <- function(x) {

  if (!is_plugins(x)) {
    abort(
      "Expecting a board plugins object to inherit from \"plugins\".",
      class = "plugins_inheritance_invalid"
    )
  }

  if (!is.list(x)) {
    abort(
      "Expecting a board plugins object behave list-like.",
      class = "plugins_type_invalid"
    )
  }

  if (!all(lgl_ply(x, is_plugin))) {
    abort(
      "Expecting a board plugins to contain a set of plugins.",
      class = "plugins_contains_invalid"
    )
  }

  if (anyDuplicated(x) != 0L) {
    abort(
      "Duplicate board plugins are not allowed.",
      class = "plugins_duplicate_invalid"
    )
  }

  x
}

#' @export
format.plugins <- function(x, ...) {
  c(
    paste0("<", class(x)[1L], "[", length(x), "]>"),
    chr_mply(paste0, names(x), ": ", chr_ply(x, format, ...))
  )
}

#' @export
print.plugins <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
names.plugins <- function(x) {
  chr_ply(x, plugin_id)
}

#' @export
`names<-.plugins` <- function(x, value) {

  reset <- is.null(value) ||
    identical(value, chr_ply(x, plugin_id)) ||
    all(is.na(value) | value == "")

  if (isTRUE(reset)) {
    attr(x, "names") <- NULL
    return(x)
  }

  abort("Cannot modify plugin names.", class = "plugin_names_assign_invalid")
}

#' @rdname board_ui
#' @export
board_ui.plugin <- function(id, x, ...) {
  call_plugin_ui(x, NS(id), ...)
}

#' @rdname board_ui
#' @export
board_ui.plugins <- function(id, x, ...) {
  do.call(
    tagList,
    lapply(x, function(y, id, ...) board_ui(id, y, ...), id, ...)
  )
}

#' @export
vec_restore.plugins <- function(x, to, ...) {
  validate_plugins(NextMethod())
}

#' @export
c.plugins <- function(...) {

  res <- unlist(
    lapply(list(...), harmonize_list_of_plugins),
    recursive = FALSE
  )

  as_plugins(res)
}

#' @export
`[.plugins` <- function(x, i, ...) {
  vec_slice(x, vec_as_location(i, length(x), chr_ply(x, plugin_id)))
}

#' @export
`[<-.plugins` <- function(x, i, ..., value) {
  abort("Cannot assign into `plugins`.", class = "plugins_assignment_invalid")
}

#' @export
`[[.plugins` <- function(x, i, ...) {
  as_plugin(
    vec_slice(x, vec_as_location2(i, length(x), chr_ply(x, plugin_id)))
  )
}

#' @export
`[[<-.plugins` <- function(x, i, ..., value) {
  abort("Cannot assign into `plugins`.", class = "plugins_assignment_invalid")
}

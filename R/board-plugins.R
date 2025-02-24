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
    class = c(class, "blockr_plugin")
  )

  validate_plugin(res)
}

#' @param x Plugin object
#' @rdname new_plugin
#' @export
is_plugin <- function(x) {
  inherits(x, "blockr_plugin") &&
    sum(inherits(x, known_plugins(), which = TRUE) != 0L) == 1L
}

known_plugins <- function() {
  c("preserve_board", "manage_blocks", "manage_links", "manage_stacks",
    "notify_user", "generate_code", "edit_block")
}

plugin_server <- function(x) x[["server"]]

plugin_ui <- function(x) x[["ui"]]

plugin_validator <- function(x) attr(x, "validator")

validate_plugin <- function(x) {

  if (!is_plugin(x)) {
    abort(
      paste0(
        "Expecting a plugin to inherit from `blockr_plugin` and one of ",
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

  if (!is.function(plugin_server(x))) {
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
  new_plugin(server, ui, validator = check_edit_block_val,
             class = "edit_block")
}

get_plugin <- function(plugin, plugins = NULL, component = NULL) {

  if (is.null(plugins)) {

    if (is.null(plugin)) {
      return(NULL)
    }

    res <- validate_plugin(plugin)

  } else {

    stopifnot(
      is_string(plugin), is.list(plugins), all(lgl_ply(plugins, is_plugin))
    )

    keys <- chr_xtr(lapply(plugins, class), 1L)

    stopifnot(anyDuplicated(keys) == 0L)

    hit <- match(plugin, keys)

    if (is.na(hit)) {
      return(NULL)
    }

    res <- validate_plugin(plugins[[hit]])
  }

  if (is.null(component)) {
    return(res)
  }

  switch(
    match.arg(component, c("server", "ui")),
    ui = plugin_ui(res),
    server = function(server_args, validator_args) {
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

#' @param which (Optional) character vectors of plugins to include
#' @rdname serve
#' @export
borad_plugins <- function(which = NULL) {

  plugins <- list(
    preserve_board(server = ser_deser_server, ui = ser_deser_ui),
    manage_blocks(server = add_rm_block_server, ui = add_rm_block_ui),
    manage_links(server = add_rm_link_server, ui = add_rm_link_ui),
    manage_stacks(server = add_rm_stack_server, ui = add_rm_stack_ui),
    notify_user(server = block_notification_server),
    generate_code(server = gen_code_server, ui = gen_code_ui),
    edit_block(server = edit_block_server, ui = edit_block_ui)
  )

  if (is.null(which)) {
    return(plugins)
  }

  stopifnot(is.character(which))

  lapply(
    which,
    get_plugin,
    plugins
  )
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

validate_plugins <- function(plugins) {

  if (!is.list(plugins)) {
    stop("Expecting a list of plugins.")
  }

  unknown <- setdiff(
    names(plugins),
    c("preserve_board", "manage_blocks", "manage_links", "manage_stacks",
      "notify_user", "generate_code", "edit_block")
  )

  if (length(unknown)) {
    abort(
      paste0("Unknown with plungin(s) ", paste_enum(unknown), "."),
      class = "unknown_board_plugin"
    )
  }

  for (plugin in plugins) {

    if (!is.list(plugin) || !setequal(names(plugin), c("server", "ui"))) {
      warn(
        paste(
          "Expecting plugings to be passed as list with components `server`",
          "and `ui`."
        ),
        class = "malformed_board_plugin"
      )
    }

    is_funs <- is.function(plugin) ||
      all(lgl_ply(Filter(Negate(is.null), plugin), is.function))

    if (!is_funs) {
      abort(
        paste(
          "Expecting plugings to be passed as list of functions (or single",
          "function, which will be deprecated)."
        ),
        class = "malformed_board_plugin"
      )
    }
  }

  invisible()
}

get_plugin <- function(plugin, plugins, which = c("server", "ui")) {

  which <- match.arg(which)

  if (plugin %in% names(plugins)) {

    res <- plugins[[plugin]]

    if (is.null(which) || !is.list(res)) {
      return(res)
    }

    stopifnot(which %in% names(res))

    return(res[[which]])
  }

  NULL
}

get_plugin_ui <- function(...) get_plugin(..., which = "ui")

get_plugin_server <- function(...) get_plugin(..., which = "server")

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

#' @param which (Optional) character vectors of plugins to include
#' @rdname serve
#' @export
borad_plugins <- function(which = NULL) {

  plugins <- list(
    preserve_board = list(
      server = ser_deser_server,
      ui = ser_deser_ui
    ),
    manage_blocks = list(
      server = add_rm_block_server,
      ui = add_rm_block_ui
    ),
    manage_links = list(
      server = add_rm_link_server,
      ui = add_rm_link_ui
    ),
    manage_stacks = list(
      server = add_rm_stack_server,
      ui = add_rm_stack_ui
    ),
    notify_user = list(
      server = block_notification_server,
      ui = NULL
    ),
    generate_code = list(
      server = gen_code_server,
      ui = gen_code_ui
    ),
    edit_block = list(
      server = edit_block_server,
      ui = edit_block_ui
    )
  )

  if (not_null(which)) {

    if (!is.character(which)) {
      abort(
        "Expecting `which` to be a character vector.",
        class = "cannot_select_board_plugin"
      )
    }

    if (!all(which %in% names(plugins))) {
      abort(
        paste0(
          "Selected unknown plugin(s)",
          paste_enum(setdiff(names(plugins), which)), "."
        ),
        class = "cannot_select_board_plugin"
      )
    }

    plugins <- plugins[which]
  }

  plugins
}

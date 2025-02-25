#' Board server
#'
#' Shiny server function for `board` objects.
#'
#' @param x Board
#' @param id (Optional) parent namespace
#' @param ... Generic consistency
#'
#' @export
board_server <- function(id, x, ...) {
  UseMethod("board_server", x)
}

#' @param plugins Board plugins as modules
#' @param callbacks Single (or list of) callback function(s), called only
#' for their side-effects)
#' @rdname board_server
#' @export
board_server.board <- function(id, x, plugins = list(), callbacks = list(),
                               ...) {

  plugins <- as_plugins(plugins)

  if (is.function(callbacks)) {
    callbacks <- list(callbacks)
  }

  validate_callbacks(callbacks)

  dot_args <- list(...)

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      board_options_to_userdata(x, input, session)

      rv <- reactiveValues(
        blocks = list(),
        inputs = list(),
        board = x,
        board_id = id,
        links = list(),
        msgs = list()
      )

      rv_lst <- list(board = make_read_only(rv))

      board_update <- reactiveVal()

      plugin_args <- c(rv_lst, list(update = board_update), dot_args)

      edit_block <- get_plugin("edit_block", plugins)

      observeEvent(
        TRUE,
        {
          rv <- setup_blocks(rv, edit_block, plugin_args)
        },
        once = TRUE
      )

      call_plugin_server(
        "manage_blocks",
        server_args = plugin_args,
        plugins = plugins
      )

      call_plugin_server(
        "manage_links",
        server_args = plugin_args,
        plugins = plugins
      )

      call_plugin_server(
        "manage_stacks",
        server_args = plugin_args,
        plugins = plugins
      )

      observeEvent(
        board_update(),
        {
          upd <- validate_board_update(board_update, rv)

          if (length(upd$blocks$add)) {

            insert_block_ui(ns(NULL), rv$board, upd$blocks$add,
                            edit_ui = edit_block)

            board_blocks(rv$board) <- c(board_blocks(rv$board), upd$blocks$add)

            for (blk in names(upd$blocks$add)) {
              rv <- setup_block(
                upd$blocks$add[[blk]], blk, rv, edit_block, plugin_args
              )
            }
          }

          if (length(upd$blocks$rm)) {

            remove_block_ui(ns(NULL), rv$board, upd$blocks$rm)

            rv <- destroy_rm_blocks(upd$blocks$rm, rv)
          }

          if (length(upd$links$add) || length(upd$links$rm)) {

            rm <- board_links(rv$board)[upd$links$rm]
            rv <- update_block_links(rv, upd$links$add, rm)

            rv$board <- modify_links(rv$board, upd$links$add, upd$links$rm)
          }

          if (length(upd$stacks$add) || length(upd$stacks$rm)) {
            rv$board <- modify_stacks(rv$board, upd$stacks$add, upd$stacks$rm)
          }

          board_update(NULL)
        }
      )

      board_refresh <- call_plugin_server(
        "preserve_board",
        server_args = plugin_args,
        plugins = plugins
      )

      if (not_null(board_refresh)) {

        observeEvent(
          board_refresh(),
          {
            log_trace("removing existing ui components")
            remove_block_ui(ns(NULL), rv$board)

            log_trace("refreshing rv$board")
            rv$board <- board_refresh()

            log_trace("updating board ui")
            update_ui(rv$board, session)

            log_trace("inserting new ui components")
            insert_block_ui(ns(NULL), rv$board, edit_ui = edit_block)

            log_trace("setting up block observers")
            rv <- setup_blocks(rv, edit_block, plugin_args)

            log_trace("completed board refresh")
          }
        )
      }

      rv$msgs <- coal(
        call_plugin_server(
          "notify_user",
          server_args = plugin_args,
          plugins = plugins
        ),
        reactive(
          filter_all_zero_len(lst_xtr_reval(rv$blocks, "server", "cond"))
        )
      )

      call_plugin_server(
        "generate_code",
        server_args = plugin_args,
        plugins = plugins
      )

      cb_res <- vector("list", length(callbacks))

      for (i in seq_along(callbacks)) {
        cb_res[[i]] <- do.call(callbacks[[i]], plugin_args)
      }

      c(rv_lst, dot_args)
    }
  )
}

setup_blocks <- function(rv, edit_mod, args) {

  stopifnot(
    is.reactivevalues(rv),
    all(c("blocks", "inputs", "board", "links") %in% names(rv)),
    is_board(rv$board)
  )

  for (link in rv$links) {
    link$destroy()
  }

  rv$blocks <- list()
  rv$inputs <- list()
  rv$links <- list()

  blks <- board_blocks(rv$board)

  for (i in names(blks)) {
    rv <- setup_block(blks[[i]], i, rv, edit_mod, args)
  }

  rv
}

setup_block <- function(blk, id, rv, mod, args) {

  arity <- block_arity(blk)
  inpts <- block_inputs(blk)

  inpts <- set_names(
    replicate(length(inpts), reactiveVal()),
    inpts
  )

  if (is.na(arity)) {
    inpts <- c(inpts, list(`...args` = reactiveValues()))
  }

  rv$inputs[[id]] <- inpts

  links <- board_links(rv$board)

  todo <- as.list(links[links$to == id])

  for (i in names(todo)) {
    rv <- do.call(
      setup_link,
      c(list(rv, i), todo[[i]])
    )
  }

  rv$blocks[[id]] <- list(
    block = blk,
    server = do.call(
      block_server,
      c(list(id, blk, rv$inputs[[id]], mod), args)
    )
  )

  rv
}

destroy_rm_blocks <- function(ids, rv) {

  links <- board_links(rv$board)

  rv <- update_block_links(
    rv,
    rm = links[links$from %in% ids | links$to %in% ids]
  )

  rv$inputs <- rv$inputs[!names(rv$inputs) %in% ids]
  rv$blocks <- rv$blocks[!names(rv$blocks) %in% ids]

  rv$board <- rm_blocks(rv$board, ids)

  rv
}

setup_link <- function(rv, id, from, to, input) {

  if (input %in% block_inputs(board_blocks(rv$board)[[to]])) {

    rv$links[[id]] <- observeEvent(
      rv$blocks[[from]]$server$result(),
      {
        rv$inputs[[to]][[input]](
          rv$blocks[[from]]$server$result()
        )
      },
      ignoreNULL = FALSE
    )

  } else {

    rv$links[[id]] <- observeEvent(
      rv$blocks[[from]]$server$result(),
      {
        rv$inputs[[to]][["...args"]][[input]] <-
          rv$blocks[[from]]$server$result()
      },
      ignoreNULL = FALSE
    )
  }

  rv
}

destroy_link <- function(rv, id, from, to, input) {

  rv$links[[id]]$destroy()
  rv$links[[id]] <- NULL

  if (input %in% block_inputs(board_blocks(rv$board)[[to]])) {
    rv$inputs[[to]][[input]](NULL)
  } else {
    rv$inputs[[to]][["...args"]][[input]] <- NULL
  }

  rv
}

update_block_links <- function(rv, add = NULL, rm = NULL) {

  todo <- as.list(rm)

  for (i in names(todo)) {
    rv <- do.call(destroy_link, c(list(rv, i), todo[[i]]))
  }

  todo <- as.list(add)

  for (i in names(todo)) {
    rv <- do.call(setup_link, c(list(rv, i), todo[[i]]))
  }

  rv
}

validate_board_update <- function(x, rv) {

  if (!is.reactive(x)) {
    abort(
      "Expecting a board update to be passed as a reactive object.",
      class = "board_update_object_invalid"
    )
  }

  res <- x()

  expected <- c("blocks", "links", "stacks")

  if (!is.list(res)) {
    abort(
      "Expecting a board update to be specified as a list.",
      class = "board_update_type_invalid"
    )
  }

  if (!all(names(res) %in% expected)) {
    abort(
      paste0(
        "Expecting a board update to consist of components ",
        paste_enum(expected), ". Please remove ",
        paste_enum(setdiff(names(res), expected)), "."
      ),
      class = "board_update_components_invalid"
    )
  }

  expected <- c("add", "rm")

  for (cmp in res) {

    if (!is.list(cmp)) {
      abort(
        "Expecting a board update component to be specified as a list.",
        class = "board_update_component_type_invalid"
      )
    }

    if (!length(names(cmp)) == length(cmp) || !all(names(cmp) %in% expected)) {

      abort(
        paste0(
          "Expecting a board update component to consist of components ",
          paste_enum(expected), ". Please remove ",
          paste_enum(setdiff(names(cmp), expected)), "."
        ),
        class = "board_update_component_components_invalid"
      )
    }
  }

  if ("blocks" %in% names(res)) {
    validate_board_update_blocks(res$blocks, rv)
  }

  if ("links" %in% names(res)) {
    validate_board_update_links(res$links, rv)
  }

  if ("stacks" %in% names(res)) {
    validate_board_update_stacks(res$stacks, rv)
  }

  res
}

validate_board_update_blocks <- function(x, rv) {

  if ("add" %in% names(x) && !is.null(x$add)) {

    if (!is_blocks(x$add)) {
      abort(
        paste(
          "Expecting the \"add\" block component of a board update",
          "to be `NULL` or a `blocks` object."
        ),
        class = "board_update_blocks_add_invalid"
      )
    }

    if (any(names(x$add) %in% board_block_ids(rv$board))) {
      abort(
        "Expecting the newly added block to have a unique ID.",
        class = "board_update_blocks_add_invalid"
      )
    }

    validate_blocks(x$add)
  }

  if ("rm" %in% names(x) && !is.null(x$rm)) {

    if (!is.character(x$rm)) {
      abort(
        paste(
          "Expecting the \"rm\" block component of a board update",
          "value to be `NULL` or a character vector."
        ),
        class = "board_update_blocks_rm_invalid"
      )
    }

    if (all(!x$rm %in% board_block_ids(rv$board))) {
      abort(
        "Expecting the removed block to be specified by a known ID.",
        class = "board_update_blocks_rm_invalid"
      )
    }
  }

  invisible()
}

validate_board_update_links <- function(x, rv) {

  if ("add" %in% names(x) && !is.null(x$add)) {

    if (!is_links(x$add)) {
      abort(
        paste(
          "Expecting the \"add\" link component of a board update",
          "value to be `NULL` or a `links` object."
        ),
        class = "board_update_links_add_invalid"
      )
    }

    if (any(names(x$add) %in% board_link_ids(rv$board))) {
      abort(
        "Expecting the newly added links to have a unique ID.",
        class = "board_update_links_add_invalid"
      )
    }

    validate_links(x$add)
  }

  if ("rm" %in% names(x) && !is.null(x$rm)) {

    if (!is.character(x$rm)) {
      abort(
        paste(
          "Expecting the \"rm\" link component of a board update",
          "to be `NULL` or a character vector."
        ),
        class = "board_update_links_rm_invalid"
      )
    }

    if (!all(x$rm %in% board_link_ids(rv$board))) {
      abort(
        "Expecting all link IDs to be removed to be known.",
        class = "board_update_links_rm_invalid"
      )
    }

  }

  invisible()
}

validate_board_update_stacks <- function(x, rv) {

  if ("add" %in% names(x) && !is.null(x$add)) {

    if (!is_stacks(x$add)) {
      abort(
        paste(
          "Expecting the \"add\" link component of a board update",
          "to be `NULL` or a `stacks` object."
        ),
        class = "board_update_stacks_add_invalid"
      )
    }

    if (any(names(x$add) %in% board_stack_ids(rv$board))) {
      abort(
        "Expecting the newly added stacks to have a unique ID.",
        class = "board_update_stacks_add_invalid"
      )
    }
    validate_stacks(x$add)
  }

  if ("rm" %in% names(x) && !is.null(x$rm)) {

    if (!is.character(x$rm)) {
      abort(
        paste(
          "Expecting the \"rm\" link component of a board update",
          "to be `NULL` or a character vector."
        ),
        class = "board_update_stacks_rm_invalid"
      )
    }

    if (!all(x$rm %in% board_stack_ids(rv$board))) {
      abort(
        "Expecting all stack IDs to be removed to be known.",
        class = "board_update_stacks_rm_invalid"
      )
    }
  }

  invisible()
}

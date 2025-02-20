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

      edit_block <- get_plugin("edit_block", plugins)

      observeEvent(
        TRUE,
        {
          rv <- setup_blocks(rv, edit_block, dot_args)
        },
        once = TRUE
      )

      board_refresh <- call_plugin_server(
        "preserve_board",
        server_args = c(list(rv), dot_args),
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
            rv <- setup_blocks(rv, edit_block, dot_args)

            log_trace("completed board refresh")
          }
        )
      }

      blocks <- call_plugin_server(
        "manage_blocks",
        server_args = c(list(rv), dot_args),
        validator_args = list(rv),
        plugins = plugins
      )

      if (not_null(blocks)) {

        observeEvent(
          blocks$add,
          {
            insert_block_ui(ns(NULL), rv$board, blocks$add,
                            edit_ui = edit_block)

            board_blocks(rv$board) <- c(board_blocks(rv$board), blocks$add)

            for (blk in names(blocks$add)) {
              rv <- setup_block(
                blocks$add[[blk]], blk, rv, edit_block, dot_args
              )
            }
          }
        )

        observeEvent(
          blocks$rm,
          {
            remove_block_ui(ns(NULL), rv$board, blocks$rm)

            rv <- destroy_rm_blocks(blocks$rm, rv)
          }
        )
      }

      links <- call_plugin_server(
        "manage_links",
        server_args = c(list(rv), dot_args),
        validator_args = list(rv),
        plugins = plugins
      )

      if (not_null(links)) {

        observeEvent(
          links(),
          {
            updates <- links()

            rm <- board_links(rv$board)[updates$rm]
            rv <- update_block_links(rv, updates$add, rm)

            rv$board <- modify_links(rv$board, updates$add, updates$rm)
          },
          ignoreInit = TRUE
        )
      }

      stacks <- call_plugin_server(
        "manage_stacks",
        server_args = c(list(rv), dot_args),
        validator_args = list(rv),
        plugins = plugins
      )

      if (not_null(stacks)) {

        observeEvent(
          stacks(),
          {
            updates <- stacks()
            rv$board <- modify_stacks(rv$board, updates$add, updates$rm)
          },
          ignoreInit = TRUE
        )
      }

      rv$msgs <- coal(
        call_plugin_server(
          "notify_user",
          server_args = c(list(rv), dot_args),
          plugins = plugins
        ),
        reactive(
          filter_all_zero_len(lst_xtr_reval(rv$blocks, "server", "cond"))
        )
      )

      call_plugin_server(
        "generate_code",
        server_args = c(list(rv), dot_args),
        plugins = plugins
      )

      cb_res <- vector("list", length(callbacks))

      for (i in seq_along(callbacks)) {
        cb_res[[i]] <- do.call(callbacks[[i]], c(list(rv), dot_args))
      }

      rv
    }
  )
}

setup_blocks <- function(rv, edit_mod, dots) {

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
    rv <- setup_block(blks[[i]], i, rv, edit_mod, dots)
  }

  rv
}

setup_block <- function(blk, id, rv, mod, dots) {

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
      c(list(id, blk, rv$inputs[[id]], mod), dots)
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

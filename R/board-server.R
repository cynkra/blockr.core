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

  validate_plugins(plugins)

  if (is.function(callbacks)) {
    callbacks <- list(callbacks)
  }

  validate_callbacks(callbacks)

  dot_args <- list(...)

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      rv <- reactiveValues(
        blocks = list(),
        inputs = list(),
        board = x,
        board_id = id,
        links = list(),
        msgs = list()
      )

      observeEvent(
        TRUE,
        {
          rv <- setup_blocks(rv)
        },
        once = TRUE
      )

      ser_deser <- get_plugin("preserve_board", plugins)

      if (not_null(ser_deser)) {
        board_refresh <- check_ser_deser_val(
          do.call(ser_deser, c(list("preserve_board", rv), dot_args))
        )

        observeEvent(
          board_refresh(),
          {
            remove_block_ui(ns(NULL), rv$board)

            rv$board <- board_refresh()

            insert_block_ui(ns(NULL), rv$board)

            rv <- setup_blocks(rv)
          }
        )
      }

      add_rm_block <- get_plugin("manage_blocks", plugins)

      if (not_null(add_rm_block)) {
        blocks <- check_add_rm_block_val(
          do.call(add_rm_block, c(list("manage_blocks", rv), dot_args)),
          rv
        )

        observeEvent(
          blocks$add,
          {
            insert_block_ui(ns(NULL), rv$board, blocks$add)

            board_blocks(rv$board) <- c(board_blocks(rv$board), blocks$add)

            for (blk in names(blocks$add)) {
              rv <- setup_block(blocks$add[[blk]], blk, rv)
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

      add_rm_link <- get_plugin("manage_links", plugins)

      if (not_null(add_rm_link)) {
        links <- check_add_rm_link_val(
          do.call(add_rm_link, c(list("manage_links", rv), dot_args)),
          rv
        )

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

      block_notifications <- get_plugin("notify_user", plugins)

      if (is.null(block_notifications)) {
        rv$msgs <- reactive(
          filter_all_zero_len(lst_xtr_reval(rv$blocks, "server", "cond"))
        )
      } else {
        rv$msgs <- check_block_notifications_val(
          do.call(
            block_notification_server,
            c(list("notify_user", rv), dot_args)
          )
        )
      }

      gen_code <- get_plugin("generate_code", plugins)

      if (not_null(gen_code)) {
        check_gen_code_val(
          do.call(gen_code, c(list("generate_code", rv), dot_args))
        )
      }

      for (callback in callbacks) {

        res <- do.call(callback, c(list(rv), dot_args))

        if (!is.null(res)) {
          warning(
            "Callbacks are only called for their side-effects and are ",
            "expected to return `NULL`."
          )
        }
      }

      rv
    }
  )
}

setup_blocks <- function(rv) {

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

  blks <- sort(rv$board)

  for (i in names(blks)) {
    rv <- setup_block(blks[[i]], i, rv)
  }

  rv
}

setup_block <- function(blk, id, rv) {

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
    rv <- do.call(setup_link, c(list(rv, i), todo[[i]]))
  }

  rv$blocks[[id]] <- list(
    block = blk,
    server = block_server(id, blk, rv$inputs[[id]])
  )

  rv
}

destroy_rm_blocks <- function(ids, rv) {

  links <- board_links(rv$board)
  blocks <- board_blocks(rv$board)

  rv <- update_block_links(
    rv,
    rm = links[links$from %in% ids | links$to %in% ids]
  )

  rv$inputs <- rv$inputs[!names(rv$inputs) %in% ids]
  rv$blocks <- rv$blocks[!names(rv$blocks) %in% ids]

  board_links(rv$board) <- links[!links$from %in% ids & !links$to %in% ids]
  board_blocks(rv$board) <- blocks[!names(blocks) %in% ids]

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

validate_plugins <- function(plugins) {

  if (!is.list(plugins)) {
    stop("Expecting a list of plugins.")
  }

  unknown <- setdiff(
    names(plugins),
    c("preserve_board", "manage_blocks", "manage_links", "notify_user",
      "generate_code")
  )

  if (length(unknown)) {
    stop("Cannot deal with plungin(s) ", paste_enum(unknown), ".")
  }

  for (plugin in plugins) {
    if (!is.function(plugin)) {
      stop("Expecting plugings to be passed as functions.")
    }
  }

  invisible()
}

get_plugin <- function(plugin, plugins) {

  if (plugin %in% names(plugins)) {
    return(plugins[[plugin]])
  }

  NULL
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

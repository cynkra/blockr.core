#' Add/remove block stacks module
#'
#' Customizable logic for adding/removing stacks grouping blocks together on
#' the board.
#'
#' @param id Namespace ID
#' @param rv Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @return A reactive value that evaualtes to `NULL` or a list with components
#' `add` and `rm`, where `add` is either `NULL` or a `stacks` object and `rm`
#' is either `NULL` or a character vector of link IDs.
#'
#' @rdname add_rm_stack
#' @export
add_rm_stack_server <- function(id, rv, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(
        TRUE,
        setup_stack_div(rv$board_id),
        once = TRUE
      )

      observeEvent(
        chr_ply(board_stacks(rv$board), stack_name),
        {
          stcks <- board_stacks(rv$board)
          map(
            assign_output,
            paste0(names(stcks), "_name"),
            chr_ply(stcks, stack_name),
            MoreArgs = list(out = output)
          )
        }
      )

      move_blocks_observer(rv, session)

      observeEvent(
        input$stacks_mod,
        showModal(stacks_modal(session$ns))
      )

      upd <- reactiveValues(
        add = stacks(),
        rm = character(),
        curr = isolate(board_stacks(rv$board)),
        obs = list(),
        edit = NULL
      )

      observeEvent(
        board_stacks(rv$board),
        {
          upd$curr <- board_stacks(rv$board)
        }
      )

      output$stacks_dt <- DT::renderDataTable(
        stack_dt(isolate(upd$curr), session$ns, isolate(rv$board)),
        server = TRUE
      )

      create_stack_obs_observer(input, rv, upd, session)

      edit_stack_observer(upd, rv)

      add_stack_observer(input, rv, upd, session)

      rm_stack_observer(input, rv, upd, session)

      cancel_stack_observer(input, rv, upd, session)

      modify_stack_observer(input, rv, upd, session)
    }
  )
}

#' @param board The initial `board` object
#' @rdname add_rm_stack
#' @export
add_rm_stack_ui <- function(id, board) {
  tagList(
    actionButton(
      NS(id, "stacks_mod"),
      "Edit stacks",
      icon = icon("stack-overflow")
    )
  )
}

assign_output <- function(key, val, out) {
  out[[key]] <- renderText(val)
  invisible()
}

setup_stack_div <- function(id) {

  deps <- htmltools::htmlDependency(
    "move-block-ui",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "moveBlockUi.js"
  )

  insertUI(
    paste0("#", id, "_blocks"),
    "afterBegin",
    tagList(div(id = paste0(id, "_stacks")), deps),
    immediate = TRUE
  )
}

move_blocks_observer <- function(rv, session) {

  rend_stacks <- reactiveVal(list())

  observeEvent(
    board_stacks(rv$board),
    {
      stks <- board_stacks(rv$board)
      to_add <- setdiff(names(stks), names(rend_stacks()))

      if (length(to_add)) {

        insert_stack_ui(rv$board_id, stks[to_add], session)

        for (i in to_add) {
          for (j in stack_blocks(stks[[i]])) {
            add_block_to_stack(j, i, session)
          }
        }

        rend_stacks(
          c(rend_stacks(), lapply(stks[to_add], stack_blocks))
        )
      }

      to_rm <- setdiff(names(rend_stacks()), names(stks))

      if (length(to_rm)) {

        for (i in unlst(rend_stacks()[to_rm])) {
          rm_block_from_stack(i, rv$board_id, session)
        }

        for (i in to_rm) {
          rm_stack_ui(i, session)
        }

        rend_stacks(setdiff(rend_stacks(), to_rm))
      }

      to_mod <- lgl_mply(
        Negate(identical),
        lapply(stks, stack_blocks),
        rend_stacks()[names(stks)]
      )

      if (any(to_mod)) {

        to_mod <- names(stks)[to_mod]

        for (i in unlst(rend_stacks()[to_mod])) {
          rm_block_from_stack(i, rv$board_id, session)
        }

        for (i in to_mod) {
          for (j in stack_blocks(stks[[i]])) {
            add_block_to_stack(j, i, session)
          }
        }
      }
    }
  )
}

insert_stack_ui <- function(id, stacks, sess) {
  insertUI(
    paste0("#", id, "_stacks"),
    "beforeEnd",
    map(empty_stack_card, stacks, names(stacks), MoreArgs = list(sess = sess)),
    immediate = TRUE,
    session = sess
  )
}

empty_stack_card <- function(x, id, sess) {
  bslib::card(
    id = paste0(id, "_stack"),
    bslib::card_header(textOutput(sess$ns(paste0(id, "_name")))),
    bslib::card_body()
  )
}

rm_stack_ui <- function(id, sess) {
  removeUI(
    paste0("#", id, "_stack"),
    immediate = TRUE,
    session = sess
  )
}

add_block_to_stack <- function(block_id, stack_id, sess) {
  sess$sendCustomMessage(
    "move-block-ui",
    list(
      sel = paste0("#", block_id, "_block"),
      dest = paste0("#", stack_id, "_stack > div.card-body")
    )
  )
}

rm_block_from_stack <- function(block_id, blocks_id, sess) {
  sess$sendCustomMessage(
    "move-block-ui",
    list(
      sel = paste0("#", block_id, "_block"),
      dest = paste0("#", blocks_id, "_blocks")
    )
  )
}

stacks_modal <- function(ns) {

  new_id_ns <- ns("new_stack_id")

  modalDialog(
    title = "Board stacks",
    DT::dataTableOutput(ns("stacks_dt")),
    footer = tagList(
      htmltools::tagQuery(
        textInput(new_id_ns, NULL, placeholder = "Next ID")
      )$addAttrs(
        style = "width: 180px; margin: 0 8px;"
      )$allTags(),
      tags$style(
        type = "text/css",
        paste0(
          "#", new_id_ns, " {padding: 0.75em 2em; margin: 4px;}"
        )
      ),
      tags$style(
        type = "text/css",
        paste0(
          "#", new_id_ns, "-label {text-align: left;}"
        )
      ),
      actionButton(ns("add_stack"), "Add", icon = icon("plus")),
      actionButton(ns("rm_stack"), "Remove", icon = icon("minus")),
      actionButton(ns("cancel_stacks"), "Cancel", class = "btn-danger"),
      actionButton(ns("modify_stacks"), "OK", class = "btn-success")
    ),
    size = "l"
  )
}

stack_dt <- function(dat, ns, board) {
  res <- DT::datatable(
    dt_board_stack(dat, ns, board),
    options = list(
      pageLength = 5,
      preDrawCallback = DT::JS(
        "function() { Shiny.unbindAll(this.api().table().node()); }"
      ),
      drawCallback = DT::JS(
        "function() { Shiny.bindAll(this.api().table().node()); }"
      ),
      dom = "tp",
      ordering = FALSE,
      columnDefs = list(
        list(targets = 0, width = "125px")
      )
    ),
    rownames = FALSE,
    escape = FALSE
  )

  DT::formatStyle(res, 1L, `vertical-align` = "middle")
}

dt_board_stack <- function(stk, ns, board) {

  ids <- names(stk)

  data.frame(
    ID = ids,
    Name = chr_mply(
      dt_text,
      lapply(paste0(ids, "_name"), ns),
      chr_ply(stk, attr, "name")
    ),
    Blocks = chr_mply(
      dt_select,
      lapply(paste0(ids, "_blocks"), ns),
      lapply(stk, as.character),
      MoreArgs = list(
        rem = available_stack_blocks(
          stacks = stk,
          blocks = board_block_ids(board)
        )
      )
    )
  )
}

dt_select <- function(id, val, rem) {

  res <- selectInput(id, label = NULL, choices = c("", val, rem),
                     selected = val, multiple = TRUE)

  res <- htmltools::tagQuery(
    res
  )$addAttrs(
    style = "width: 400px; margin-bottom: 0;"
  )$allTags()

  as.character(res)
}

dt_text <- function(id, val) {

  res <- textInput(id, label = NULL, value = val)

  res <- htmltools::tagQuery(
    res
  )$addAttrs(
    style = "width: 200px; margin-bottom: 0;"
  )$allTags()

  as.character(res)
}

create_dt_stack_obs <- function(ids, upd, ...) {

  create_obs <- function(col, row, upd, input, blks, sess) {

    inp <- paste0(row, "_", col)

    log_debug("creating stack DT observer ", inp)

    observeEvent(
      input[[inp]],
      {
        new <- input[[inp]]

        if (col == "blocks" && setequal(stack_blocks(upd$curr[[row]]), new)) {
          return()
        }

        if (col == "name" && identical(stack_name(upd$curr[[row]]), new)) {
          return()
        }

        if (col == "blocks") {

          if (is.null(new)) {
            new <- character()
          }

          rem <- setdiff(names(upd$curr), row)
          ava <- available_stack_blocks(
            stacks = c(upd$curr[rem], new),
            blocks = names(blks)
          )

          for (i in rem) {

            cur <- stack_blocks(upd$curr[[i]])

            updateSelectInput(
              sess,
              paste0(i, "_blocks"),
              label = NULL,
              choices = c(cur, ava),
              selected = cur
            )
          }

        } else if (col != "name") {

          stop("Unexpected input: column ", col)
        }

        upd$edit <- list(row = row, col = col, val = new)
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )
  }

  create_obs_for_id <- function(id, ...) {
    lapply(
      set_names(nm = c("name", "blocks")),
      create_obs,
      id,
      ...
    )
  }

  upd$obs[ids] <- lapply(ids, create_obs_for_id, upd, ...)

  upd
}

destroy_dt_stack_obs <- function(ids, update) {

  for (row in ids) {
    for (col in c("name", "blocks")) {
      log_debug("destroying stack DT observer ", row, " ", col)
      update$obs[[row]][[col]]$destroy()
    }
    update$obs[[row]] <- NULL
  }

  update
}

create_stack_obs_observer <- function(input, rv, upd, sess) {

  stacks_proxy <- DT::dataTableProxy("stacks_dt", sess)

  observeEvent(
    upd$curr,
    {
      ids <- names(upd$curr)

      DT::replaceData(
        stacks_proxy,
        dt_board_stack(upd$curr, sess$ns, rv$board),
        rownames = FALSE
      )

      upd <- create_dt_stack_obs(setdiff(ids, names(upd$obs)), upd, input,
                                 board_blocks(rv$board), sess)
      upd <- destroy_dt_stack_obs(setdiff(names(upd$obs), ids), upd)
    }
  )
}

edit_stack_observer <- function(upd, rv) {

  observeEvent(
    upd$edit,
    {
      row <- upd$edit$row
      col <- upd$edit$col
      val <- upd$edit$val

      if (!row %in% upd$rm && row %in% board_stack_ids(rv$board)) {
        upd$rm <- c(upd$rm, row)
      }

      if (col == "name") {
        stack_name(upd$curr[[row]]) <- val
      } else if (col == "blocks") {
        stack_blocks(upd$curr[[row]]) <- val
      }

      if (row %in% names(upd$add)) {
        upd$add[row] <- upd$curr[row]
      } else {
        upd$add <- c(upd$add, upd$curr[row])
      }
    }
  )
}

add_stack_observer <- function(input, rv, upd, sess) {

  observeEvent(
    input$add_stack,
    {
      avail <- available_stack_blocks(
        stacks = upd$curr,
        blocks = board_block_ids(rv$board)
      )

      if (length(avail)) {

        new <- new_stack()

        if (length(input$new_stack_id) && nchar(input$new_stack_id)) {

          updateTextInput(
            session = sess,
            inputId = "new_stack_id",
            label = NULL,
            value = "",
            placeholder = "Next ID"
          )

          if (input$new_stack_id %in% names(upd$curr)) {
            showNotification(
              "Please choose a unique link ID.",
              type = "warning"
            )
            return()
          }

          new <- set_names(list(new), input$new_stack_id)
        }

        upd$curr <- c(upd$curr, new)
        upd$add <- c(upd$add, upd$curr[length(upd$curr)])

      } else {

        showNotification(
          "No new stacks can be added. Remove a row first.",
          type = "warning"
        )
      }
    }
  )
}

rm_stack_observer <- function(input, rv, upd, sess) {

  observeEvent(
    input$rm_stack,
    {
      sel <- input$stacks_dt_rows_selected

      if (length(sel)) {

        ids <- names(upd$curr[sel])

        upd$rm <- c(upd$rm, ids[ids %in% board_stack_ids(rv$board)])

        upd$add <- upd$add[setdiff(names(upd$add), ids)]
        upd$curr <- upd$curr[setdiff(names(upd$curr), ids)]

      } else {

        showNotification("No row selected", type = "warning", session = sess)
      }
    }
  )
}

cancel_stack_observer <- function(input, rv, upd, session) {

  observeEvent(
    input$cancel_stacks,
    {
      removeModal(session)

      upd$add <- stacks()
      upd$rm <- character()
      upd$curr <- board_stacks(rv$board)
    }
  )
}

modify_stack_observer <- function(input, rv, upd, sess, res) {

  res <- reactiveVal(
    list(add = NULL, rm = NULL)
  )

  observeEvent(
    input$modify_stacks,
    {
      if (!length(upd$add) && !length(upd$rm)) {

        showNotification(
          "No changes specified.",
          type = "warning"
        )

        return()
      }

      new <- tryCatch(
        modify_stacks(rv$board, upd$add, upd$rm),
        warning = function(e) {
          showNotification(conditionMessage(e), duration = NULL,
                           type = "warning")
        },
        error = function(e) {
          showNotification(conditionMessage(e), duration = NULL,
                           type = "error")
        }
      )

      res(
        list(
          add = if (length(upd$add)) upd$add else stacks(),
          rm = if (length(upd$rm)) upd$rm else character()
        )
      )

      upd$add <- stacks()
      upd$rm <- character()
      upd$curr <- board_stacks(new)

      removeModal(sess)
    }
  )

  res
}

check_add_rm_stack_val <- function(val, rv) {

  observeEvent(
    TRUE,
    {
      if (!is.reactive(val)) {
        abort(
          "Expecting `manage_stacks` to return a reactive value.",
          class = "manage_stacks_return_invalid"
        )
      }
    },
    once = TRUE,
    priority = 4
  )

  observeEvent(
    val(),
    {
      if (!is.list(val()) || !setequal(names(val()), c("add", "rm"))) {
        abort(
          paste(
            "Expecting the `manage_stacks` return value to evaluate to a list",
            "with components `add` and `rm`."
          ),
          class = "manage_stacks_return_invalid"
        )
      }
    },
    once = TRUE,
    priority = 3
  )

  observeEvent(
    val()$add,
    {
      if (!is_stacks(val()$add)) {
        abort(
          paste(
            "Expecting the `add` component of the `manage_stacks` return",
            "value to be `NULL` or a `stacks` object."
          ),
          class = "manage_stacks_return_invalid"
        )
      }
    },
    once = TRUE,
    priority = 2
  )

  observeEvent(
    val()$add,
    validate_stacks(val()$add),
    priority = 1
  )

  observeEvent(
    val()$rm,
    {
      if (!is.character(val()$rm)) {
        abort(
          paste(
            "Expecting the `rm` component of the `manage_stacks` return",
            "value to be a character vector."
          ),
          class = "manage_stacks_return_invalid"
        )
      }
    },
    once = TRUE,
    priority = 2
  )

  observeEvent(
    val()$rm,
    {
      if (!all(val()$rm %in% board_stack_ids(rv$board))) {
        abort(
          "Expecting all stack IDs to be removed to be known.",
          class = "manage_stacks_return_invalid"
        )
      }
    },
    priority = 1
  )

  val
}

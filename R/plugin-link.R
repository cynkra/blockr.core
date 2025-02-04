#' Add/remove block links module
#'
#' Customizable logic for adding/removing links between blocks on the
#' board.
#'
#' @param id Namespace ID
#' @param rv Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @return A reactive value that evaualtes to `NULL` or a list with components
#' `add` and `rm`, where `add` is either `NULL` or a `data.frame` with columns
#' `id`, `from`, `tp` and `input` and `rm` is either `NULL` or a character
#' vector of link IDs.
#'
#' @rdname add_rm_link
#' @export
add_rm_link_server <- function(id, rv, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(
        input$links_mod,
        showModal(links_modal(session$ns))
      )

      upd <- reactiveValues(
        add = links(),
        rm = character(),
        curr = isolate(board_links(rv$board)),
        obs = list(),
        edit = NULL
      )

      observeEvent(
        board_links(rv$board),
        {
          upd$curr <- board_links(rv$board)
        }
      )

      output$links_dt <- DT::renderDT(
        {
          DT::datatable(
            dt_board_link(isolate(upd$curr), session$ns, rv),
            options = list(
              pageLength = 5,
              preDrawCallback = DT::JS(
                "function() { Shiny.unbindAll(this.api().table().node()); }"
              ),
              drawCallback = DT::JS(
                "function() { Shiny.bindAll(this.api().table().node()); }"
              ),
              dom = "tp",
              ordering = FALSE
            ),
            rownames = FALSE,
            escape = FALSE
          )
        },
        server = TRUE
      )

      proxy <- DT::dataTableProxy("links_dt")

      observeEvent(
        names(upd$curr),
        {
          ids <- names(upd$curr)

          DT::replaceData(
            proxy,
            dt_board_link(upd$curr, session$ns, rv),
            rownames = FALSE
          )

          upd <- create_dt_observers(setdiff(ids, names(upd$obs)), input, upd,
                                     board_blocks(rv$board), session)
          upd <- destroy_dt_observers(setdiff(names(upd$obs), ids), upd)
        }
      )

      edit_link_observer(upd, rv)

      add_link_observer(input, rv, upd)

      rm_link_observer(input, rv, upd)

      cancel_link_observer(session, input, rv, upd)

      res <- reactiveVal(
        list(add = NULL, rm = NULL)
      )

      observeEvent(
        input$modify_links,
        {
          if (!length(upd$add) && !length(upd$rm)) {

            showNotification(
              "No changes specified.",
              type = "warning"
            )

            return()
          }

          new <- tryCatch(
            modify_links(rv$board, upd$add, upd$rm),
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
              add = if (length(upd$add)) upd$add else links(),
              rm = if (length(upd$rm)) upd$rm else character()
            )
          )

          upd$add <- links()
          upd$rm <- character()
          upd$curr <- board_links(new)

          removeModal()
        }
      )

      res
    }
  )
}

#' @param board The initial `board` object
#' @rdname add_rm_link
#' @export
add_rm_link_ui <- function(id, board) {
  tagList(
    actionButton(
      NS(id, "links_mod"),
      "Edit links",
      icon = icon("table")
    ),
    slow_text_input_binding()
  )
}

dt_board_link <- function(lnk, ns, rv) {

  blks <- board_blocks(rv$board)
  arity <- int_ply(blks, block_arity, use_names = TRUE)

  from_ids <- rep(list(names(blks)), length(lnk))

  to_avail <- arity
  cnt_to <- c(table(filter_empty(lnk$to)))

  to_avail[names(cnt_to)] <- int_mply(`-`, to_avail[names(cnt_to)], cnt_to)
  to_avail[is.na(to_avail)] <- 1L

  to_avail <- lapply(
    lapply(lnk$to, filter_empty),
    union,
    names(to_avail)[to_avail > 0L]
  )

  cnt_to <- table(lnk$to)[names(blks[is.na(arity)])]
  cnt_to[is.na(cnt_to)] <- 0L

  cur_inp <- split(lnk$input, lnk$to)

  in_avail <- c(
    lapply(blks[!is.na(arity)], block_inputs),
    Map(
      union,
      lapply(lapply(cnt_to, seq_len), as.character),
      cur_inp[names(cnt_to)]
    )
  )

  rm_inp <- lapply(
    seq_along(lnk),
    function(i) split(lnk$input[-i], lnk$to[-i])[[lnk$to[i]]]
  )

  data.frame(
    ID = chr_mply(
      dt_text,
      lapply(paste0(lnk$id, "_id"), ns),
      lnk$id
    ),
    From = chr_mply(
      dt_selectize,
      lapply(paste0(lnk$id, "_from"), ns),
      lnk$from,
      Map(setdiff, from_ids, lnk$to)
    ),
    To = chr_mply(
      dt_selectize,
      lapply(paste0(lnk$id, "_to"), ns),
      lnk$to,
      Map(setdiff, to_avail, lnk$from)
    ),
    Input = chr_mply(
      dt_selectize,
      lapply(paste0(lnk$id, "_input"), ns),
      lnk$input,
      Map(setdiff, in_avail[lnk$to], rm_inp),
      is.na(arity[lnk$to])
    )
  )
}

dt_text <- function(id, val) {
  as.character(
    slow_text_input(inputId = id, label = "", value = val, width = "150px",
                    debounce = 5000)
  )
}

dt_selectize <- function(id, val, choices, create = FALSE) {

  if (isTRUE(create)) {
    opts <- list(create = TRUE)
  } else {
    opts <- NULL
  }

  res <- selectizeInput(id, label = "", choices = c("", choices),
                        selected = val, options = opts, width = "150px")

  as.character(res)
}

create_dt_observers <- function(ids, input, update, blks, sess) {
  update$obs[ids] <- lapply(ids, create_dt_observers_for_id, input, update,
                            blks, sess)
  update
}

create_dt_observers_for_id <- function(id, input, update, blks, sess) {

  obs <- set_names(nm = c("id", "from", "to", "input"))

  lapply(obs, create_dt_observer, id, input, update, blks, sess)
}

create_dt_observer <- function(col, row, input, upd, blks, sess) {

  inp <- paste0(row, "_", col)

  log_debug("creating link DT observer ", inp)

  observeEvent(
    input[[inp]],
    {
      new <- input[[inp]]

      if (col == "id") {
        cur <- row
      } else {
        cur <- upd$curr[[row]][[col]]
      }

      if (new == cur || new == "") {
        return()
      }

      if (col == "id") {

        if (new %in% names(upd$curr)) {

          showNotification(
            "Please choose a unique link ID.",
            type = "warning"
          )

          return()
        }

      } else if (col == "from") {

        to_avail <- int_ply(blks, block_arity, use_names = TRUE)

        cnt <- c(table(filter_empty(upd$curr$to)))

        to_avail[names(cnt)] <- int_mply(`-`, to_avail[names(cnt)], cnt)
        to_avail[is.na(to_avail)] <- 1L

        to_avail <- c(
          upd$curr[[row]][["to"]],
          names(to_avail)[to_avail > 0L]
        )

        updateSelectizeInput(
          sess,
          inputId = paste0(row, "_to"),
          choices = c("", setdiff(to_avail, new)),
          selected = upd$curr[[row]][["to"]]
        )

      } else if (col == "to") {

        ids <- names(blks)

        updateSelectizeInput(
          sess,
          inputId = paste0(row, "_from"),
          choices = c("", setdiff(ids, new)),
          selected = upd$curr[[row]][["from"]]
        )

        if (identical(new, "")) {

          updateSelectizeInput(
            sess,
            inputId = paste0(row, "_input"),
            choices = list()
          )

        } else {

          blk <- blks[[which(ids == new)]]
          ary <- block_arity(blk)
          hit <- upd$curr$to == new

          if (is.na(ary)) {
            inp <- as.character(sum(hit) + 1L)
            opt <- list(create = TRUE)
          } else {
            inp <- setdiff(block_inputs(blk), upd$curr$input[hit])
            opt <-  NULL
          }

          updateSelectizeInput(
            sess,
            inputId = paste0(row, "_input"),
            choices = c("", inp),
            selected = upd$curr[[row]][["input"]],
            options = opt
          )
        }

      } else if (col != "input") {

        stop("Unexpected input: column ", col)
      }

      upd$edit <- list(row = row, col = col, val = new)
    },
    ignoreInit = TRUE
  )
}

destroy_dt_observers <- function(ids, update) {

  for (row in ids) {
    for (col in c("id", "from", "to", "input")) {
      update$obs[[row]][[col]]$destroy()
    }
    update$obs[[row]] <- NULL
  }

  update
}

links_modal <- function(ns) {
  modalDialog(
    title = "Board links",
    DT::dataTableOutput(ns("links_dt")),
    footer = tagList(
      actionButton(ns("add_link"), "Add row", icon = icon("plus")),
      actionButton(ns("rm_link"), "Remove selected", icon = icon("minus")),
      actionButton(ns("cancel_links"), "Cancel", class = "btn-danger"),
      actionButton(ns("modify_links"), "OK", class = "btn-success")
    ),
    size = "l"
  )
}

edit_link_observer <- function(upd, rv) {

  observeEvent(upd$edit, {

    row <- upd$edit$row
    col <- upd$edit$col

    if (!row %in% upd$rm && row %in% board_link_ids(rv$board)) {
      upd$rm <- c(upd$rm, row)
    }

    new <- do.call(
      `$<-`,
      list(upd$curr[row], col, coal(upd$edit$val, ""))
    )

    if (col == "id") {
      upd$curr[row] <- NULL
      upd$curr <- c(upd$curr, new)
    } else {
      upd$curr[row] <- new
    }

    exst <- row %in% names(upd$add)

    if (exst && col == "id") {
      upd$add[row] <- NULL
      upd$add <- c(upd$add, new)
    } else if (exst) {
      upd$add[row] <- new
    } else {
      upd$add <- c(upd$add, new)
    }
  })
}

add_link_observer <- function(input, rv, upd) {

  observeEvent(input$add_link, {

    total <- sum(block_arity(rv$board))

    if (is.na(total) || length(upd$curr) < total) {

      upd$curr <- c(
        upd$curr,
        new_link(from = "", to = "", input = "")
      )

      upd$add <- c(upd$add, upd$curr[length(upd$curr)])

    } else {

      showNotification(
        "No new links can be added. Remove a row first.",
        type = "warning"
      )
    }
  })
}

rm_link_observer <- function(input, rv, upd) {

  observeEvent(input$rm_link, {

    sel <- input$links_rows_selected

    if (length(sel)) {

      ids <- names(upd$curr[sel])

      upd$rm <- c(upd$rm, ids[ids %in% board_link_ids(rv$board)])

      upd$add <- upd$add[setdiff(names(upd$add), ids)]
      upd$curr <- upd$curr[setdiff(names(upd$curr), ids)]

    } else {

      showNotification("No row selected", type = "warning")
    }
  })
}

cancel_link_observer <- function(session, input, rv, upd) {

  observeEvent(input$cancel_links, {

    removeModal(session)

    upd$add <- links()
    upd$rm <- character()
    upd$curr <- board_links(rv$board)
  })
}

check_add_rm_link_val <- function(val, rv) {

  observeEvent(
    TRUE,
    {
      if (!is.reactive(val)) {
        abort(
          "Expecting `add_rm_link` to return a reactive value.",
          class = ""
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
        stop("Expecting the `add_rm_link` return value to evaluate to a list ",
             "with components `add` and `rm`.")
      }
    },
    once = TRUE,
    priority = 3
  )

  observeEvent(
    val()$add,
    {
      if (!is_links(val()$add)) {
        stop("Expecting the `add` component of the `add_rm_link` return ",
             "value to be `NULL` or a `links` object.")
      }
    },
    once = TRUE,
    priority = 2
  )

  observeEvent(
    val()$add,
    validate_links(val()$add),
    priority = 1
  )

  observeEvent(
    val()$rm,
    {
      if (!is.character(val()$rm)) {
        stop("Expecting the `rm` component of the `add_rm_link` return ",
             "value to be a character vector.")
      }
    },
    once = TRUE,
    priority = 1
  )

  observeEvent(
    val()$rm,
    {
      if (!all(val()$rm %in% board_link_ids(rv$board))) {
        stop("Expecting all link IDs to be removed to be known.")
      }
    },
    priority = 1
  )

  val
}

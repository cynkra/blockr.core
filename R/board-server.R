#' Board server
#'
#' Shiny server function for `board` objects.
#'
#' @param x Board
#' @param ... Generic consistency
#'
#' @export
board_server <- function(x, ...) {
  UseMethod("board_server")
}

#' @param ser_deser Module for serialization/deserialization
#' @rdname board_server
#' @export
board_server.board <- function(x,
                               ser_deser = NULL,
                               add_rm_block = NULL,
                               ...) {
  moduleServer(
    board_id(x),
    function(input, output, session) {

      rv <- reactiveValues(
        blocks = list(),
        inputs = list(),
        board = x,
        links = list()
      )

      observeEvent(
        TRUE,
        {
          rv <- setup_blocks(rv)
        },
        once = TRUE
      )

      if (not_null(ser_deser)) {

        board_refresh <- ser_deser(rv)

        observeEvent(
          board_refresh(),
          {
            removeUI(
              paste0("#", board_id(rv$board), "_blocks > div")
            )

            rv$board <- board_refresh()

            insertUI(
              paste0("#", board_id(rv$board), "_blocks"),
              "afterBegin",
              block_cards(rv$board)
            )

            rv <- setup_blocks(rv)
          },
          ignoreInit = TRUE
        )
      }

      if (not_null(add_rm_block)) {

        block <- add_rm_block(rv)

        observeEvent(
          block$add,
          {
            insertUI(
              paste0("#", board_id(rv$board), "_blocks"),
              "beforeEnd",
              block_card(block$add, board_id(rv$board))
            )

            rv$board <- add_block(rv$board, block$add)

            rv <- setup_block(block$add, rv)
          },
          ignoreInit = TRUE
        )

        observeEvent(
          block$rm,
          {
            removeUI(
              paste0("#", block$rm, "_block")
            )

            rv <- destroy_block(block$rm, rv)
          },
          ignoreInit = TRUE
        )
      }


      observeEvent(input$links, {
        showModal(links_modal(session$ns))
      })

      link_updates <- reactiveValues(
        add = NULL,
        rm = NULL,
        curr = isolate(board_links(rv$board)),
        obs = list(),
        edit = NULL
      )

      observeEvent(board_links(rv$board), {
        link_updates$curr <- board_links(rv$board)
      })

      output$links <- DT::renderDT(
        {
          dat <- data.frame(
            From = apply(link_updates$curr, 1L, dt_selectize, session$ns,
                         "from", choices = block_ids(rv$board)),
            To = apply(link_updates$curr, 1L, dt_selectize, session$ns, "to",
                       choices = block_ids(rv$board)),
            Input = mapply(
              dt_selectize,
              split(link_updates$curr, seq_len(nrow(link_updates$curr))),
              choices = block_inputs(rv$board)[link_updates$curr$to],
              MoreArgs = list(ns = session$ns, target = "input")
            )
          )

          DT::datatable(
            dat,
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
            escape = FALSE,
            editable = TRUE
          )
        },
        server = TRUE
      )

      observeEvent(
        link_updates$curr$id,
        {
          to_add <- setdiff(link_updates$curr$id, names(link_updates$obs))

          link_updates$obs[to_add] <- lapply(
            to_add,
            function(row) {
              lapply(
                set_names(nm = c("from", "to", "input")),
                function(col) {
                  observeEvent(
                    input[[paste0(row, "_", col)]],
                    {
                      cur <- link_updates$curr
                      cur <- cur[cur$id == row, col]
                      val <- input[[paste0(row, "_", col)]]
                      if (val != cur) {
                        link_updates$edit <- list(row = row, col = col,
                                                  val = val)
                      }
                    },
                    ignoreInit = TRUE
                  )
                }
              )
            }
          )

          to_rm <- setdiff(names(link_updates$obs), link_updates$curr$id)

          for (row in to_rm) {
            for (col in c("from", "to", "input")) {
              link_updates$obs[[row]][[col]]$destroy()
            }
            link_updates$obs[[row]] <- NULL
          }
        }
      )

      observeEvent(link_updates$edit, {

        row <- link_updates$edit$row
        col <- link_updates$edit$col

        if (!row %in% link_updates$row && row %in% board_links(rv$board)$id) {
          link_updates$row <- c(link_updates$row, row)
        }

        link_updates$curr[link_updates$curr$id == row, col] <- coal(
          link_updates$edit$val,
          ""
        )

        new <- link_updates$curr[link_updates$curr$id == row, ]

        if (row %in% link_updates$add$id) {
          link_updates$add[link_updates$add$id == row, ] <- new
        } else {
          link_updates$add <- rbind(link_updates$add, new)
        }
      })

      observeEvent(input$add_link, {

        new <- data.frame(
          id = rand_names(link_updates$curr$id),
          from = "",
          to = "",
          input = ""
        )

        link_updates$curr <- rbind(link_updates$curr, new)
        link_updates$add <- rbind(link_updates$add, new)
      })

      observeEvent(input$rm_link, {

        sel <- input$links_rows_selected

        if (length(sel)) {

          id <- link_updates$curr[sel, "id"]

          if (id %in% board_links(rv$board)$id) {
            link_updates$rm <- c(link_updates$rm, id)
          } else {
            to_rm <- which(link_updates$add$id == id)
            link_updates$add <- link_updates$add[-to_rm, ]
          }

          link_updates$curr <- link_updates$curr[-sel, ]

        } else {
          showNotification("No row selected", type = "warning")
        }
      })

      observeEvent(input$cancel_links, {
        removeModal()
        link_updates$curr <- board_links(rv$board)
      })

      observeEvent(input$modify_links, {
        rv <- tryCatch(
          {
            if ((is.null(link_updates$add) || !nrow(link_updates$add)) &&
                (is.null(link_updates$rm) || !length(link_updates$rm))) {

              showNotification(
                "No changes specified.",
                type = "warning"
              )

            } else {

              new <- modify_links(rv$board, link_updates$add, link_updates$rm)
              old <- board_links(rv$board)

              rv <- update_block_connections(
                rv,
                add = link_updates$add,
                rm = old[old$id %in% link_updates$rm, ]
              )

              link_updates$add <- NULL
              link_updates$rm <- NULL
              link_updates$curr <- board_links(new)

              rv$board <- new
            }

            removeModal()

            rv
          },
          error = function(e) {
            showNotification(conditionMessage(e), duration = NULL,
                             type = "error")
          }
        )
      })

      list(
        board = reactive(rv$board),
        blocks = reactive(rv$blocks)
      )
    }
  )
}

dt_selectize <- function(row, ns, target, choices) {
  as.character(
    selectInput(
      ns(paste0(row[["id"]], "_", target)),
      label = "",
      choices = c("", choices),
      selected = row[[target]]
    )
  )
}

update_block_connections <- function(rv, add = NULL, rm = NULL) {

  if (not_null(rm)) {
    for (i in seq_len(nrow(rm))) {
      rv <- do.call(destroy_connection, c(list(rv), rm[i, ]))
    }
  }

  if (not_null(add)) {
    for (i in seq_len(nrow(add))) {
      rv <- do.call(setup_connection, c(list(rv), add[i, ]))
    }
  }

  rv
}

links_modal <- function(ns) {
  modalDialog(
    title = "Board connections",
    DT::dataTableOutput(ns("links")),
    footer = tagList(
      actionButton(ns("add_link"), "Add row", icon = icon("plus")),
      actionButton(ns("rm_link"), "Remove selected", icon = icon("minus")),
      actionButton(ns("cancel_links"), "Cancel", class = "btn-danger"),
      actionButton(ns("modify_links"), "OK", class = "btn-success")
    ),
    size = "xl"
  )
}

setup_blocks <- function(rv) {

  stopifnot(
    is.reactivevalues(rv),
    setequal(names(rv), c("blocks", "inputs", "board", "links")),
    is_board(rv$board)
  )

  for (link in rv$links) {
    link$destroy()
  }

  rv$blocks <- list()
  rv$inputs <- list()
  rv$links <- list()

  for (blk in sort(rv$board)) {
    rv <- setup_block(blk, rv)
  }

  rv
}

setup_block <- function(blk, rv) {

  id <- block_uid(blk)

  rv$inputs[[id]] <- set_names(
    replicate(block_arity(blk), reactiveVal()),
    block_inputs(blk)
  )

  links <- board_links(rv$board)

  for (i in which(links$to == id)) {
    rv <- do.call(setup_connection, c(list(rv), links[i, ]))
  }

  rv$blocks[[id]] <- list(
    block = blk,
    server = block_server(
      blk,
      data = rv$inputs[[id]]
    )
  )

  rv
}

setup_connection <- function(rv, id, from, to, input) {

  rv$links[[id]] <- observeEvent(
    rv$blocks[[from]]$server$result(),
    {
      rv$inputs[[to]][[input]](
        rv$blocks[[from]]$server$result()
      )
    }
  )

  rv
}

destroy_connection <- function(rv, id, from, to, input) {

  rv$links[[id]]$destroy()
  rv$links[[id]] <- NULL

  rv$inputs[[to]][[input]](NULL)

  rv
}

destroy_block <- function(id, rv) {

  links <- board_links(rv$board)

  for (row in which(links$from %in% id | links$to %in% id)) {
    rv <- do.call(destroy_connection, c(list(rv), links[row, ]))
  }

  rv$inputs[[id]] <- NULL
  rv$blocks[[id]] <- NULL

  rv$board <- remove_blocks(rv$board, id)

  rv
}

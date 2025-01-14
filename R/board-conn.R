#' Add/remove block connections module
#'
#' Customizable logic for adding/removing connections between blocks on the
#' board.
#'
#' @param rv Reactive values object
#'
#' @return A reactive value that evaualtes to `NULL` or a list with components
#' `add` and `rm`, where `add` is either `NULL` or a `data.frame` with columns
#' `id`, `from`, `tp` and `input` and `rm` is either `NULL` or a character
#' vector of connection IDs.
#'
#' @rdname add_rm_conn
#' @export
add_rm_conn_server <- function(rv) {
  moduleServer(
    "add_rm_conn",
    function(input, output, session) {

      observeEvent(input$links, {
        showModal(links_modal(session$ns))
      })

      conn_updates <- reactiveValues(
        add = NULL,
        rm = NULL,
        curr = isolate(board_links(rv$board)),
        obs = list(),
        edit = NULL
      )

      observeEvent(board_links(rv$board), {
        conn_updates$curr <- board_links(rv$board)
      })

      output$links <- DT::renderDT(
        {
          dat <- data.frame(
            From = apply(conn_updates$curr, 1L, dt_selectize, session$ns,
                         "from", choices = block_ids(rv$board)),
            To = apply(conn_updates$curr, 1L, dt_selectize, session$ns, "to",
                       choices = block_ids(rv$board)),
            Input = mapply(
              dt_selectize,
              split(conn_updates$curr, seq_len(nrow(conn_updates$curr))),
              choices = block_inputs(rv$board)[conn_updates$curr$to],
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
        conn_updates$curr$id,
        {
          to_add <- setdiff(conn_updates$curr$id, names(conn_updates$obs))

          conn_updates$obs[to_add] <- lapply(
            to_add,
            function(row) {
              lapply(
                set_names(nm = c("from", "to", "input")),
                function(col) {
                  observeEvent(
                    input[[paste0(row, "_", col)]],
                    {
                      cur <- conn_updates$curr
                      cur <- cur[cur$id == row, col]
                      val <- input[[paste0(row, "_", col)]]
                      if (val != cur) {
                        conn_updates$edit <- list(row = row, col = col,
                                                  val = val)
                      }
                    },
                    ignoreInit = TRUE
                  )
                }
              )
            }
          )

          to_rm <- setdiff(names(conn_updates$obs), conn_updates$curr$id)

          for (row in to_rm) {
            for (col in c("from", "to", "input")) {
              conn_updates$obs[[row]][[col]]$destroy()
            }
            conn_updates$obs[[row]] <- NULL
          }
        }
      )

      observeEvent(conn_updates$edit, {

        row <- conn_updates$edit$row
        col <- conn_updates$edit$col

        if (!row %in% conn_updates$row && row %in% board_links(rv$board)$id) {
          conn_updates$row <- c(conn_updates$row, row)
        }

        conn_updates$curr[conn_updates$curr$id == row, col] <- coal(
          conn_updates$edit$val,
          ""
        )

        new <- conn_updates$curr[conn_updates$curr$id == row, ]

        if (row %in% conn_updates$add$id) {
          conn_updates$add[conn_updates$add$id == row, ] <- new
        } else {
          conn_updates$add <- rbind(conn_updates$add, new)
        }
      })

      observeEvent(input$add_link, {

        new <- data.frame(
          id = rand_names(conn_updates$curr$id),
          from = "",
          to = "",
          input = ""
        )

        conn_updates$curr <- rbind(conn_updates$curr, new)
        conn_updates$add <- rbind(conn_updates$add, new)
      })

      observeEvent(input$rm_link, {

        sel <- input$links_rows_selected

        if (length(sel)) {

          id <- conn_updates$curr[sel, "id"]

          if (id %in% board_links(rv$board)$id) {
            conn_updates$rm <- c(conn_updates$rm, id)
          } else {
            to_rm <- which(conn_updates$add$id == id)
            conn_updates$add <- conn_updates$add[-to_rm, ]
          }

          conn_updates$curr <- conn_updates$curr[-sel, ]

        } else {
          showNotification("No row selected", type = "warning")
        }
      })

      observeEvent(input$cancel_links, {
        removeModal()
        conn_updates$curr <- board_links(rv$board)
      })

      res <- reactiveVal(
        list(add = NULL, rm = NULL)
      )

      observeEvent(
        input$modify_links,
        {
          if ((is.null(conn_updates$add) || !nrow(conn_updates$add)) &&
            (is.null(conn_updates$rm) || !length(conn_updates$rm))) {

            showNotification(
              "No changes specified.",
              type = "warning"
            )

            return()
          }

          new <- tryCatch(
            modify_links(rv$board, conn_updates$add, conn_updates$rm),
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
            list(add = conn_updates$add, rm = conn_updates$rm)
          )

          conn_updates$add <- NULL
          conn_updates$rm <- NULL
          conn_updates$curr <- board_links(new)

          removeModal()
        }
      )

      res
    }
  )
}

#' @param rv Namespace ID
#' @param board The initial `board` object
#' @rdname add_rm_conn
#' @export
add_rm_conn_ui <- function(id, board) {

  ns <- NS(
    NS(id, "add_rm_conn")
  )

  list(
    actionButton(
      ns("links"),
      "Edit connections",
      icon = icon("table")
    )
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

check_add_rm_conn_val <- function(val, rv) {

  observeEvent(
    TRUE,
    {
      if (!is.reactive(val)) {
        stop("Expecting `add_rm_conn` to return a reactive value.")
      }
    },
    once = TRUE
  )

  observeEvent(
    val(),
    {
      if (!is.list(val()) || !setequal(names(val()), c("add", "rm"))) {
        stop("Expecting the `add_rm_conn` return value to evaluate to a list ",
             "with components `add` and `rm`.")
      }
    },
    once = TRUE
  )

  observeEvent(
    val()$add,
    {
      if (!is.data.frame(val()$add)) {
        stop("Expecting the `add` component of the `add_rm_conn` return ",
             "value to be `NULL` or a `data.frame`.")
      }

      if (!all(c("id", "from", "to", "input") %in% colnames(val()$add))) {
        stop("Expecting the `add` component of the `add_rm_conn` return ",
             "value to contain columns `id`, `from`, `to` and `input`.")
      }
    },
    once = TRUE
  )

  observeEvent(
    val()$add,
    {
      if (any(val()$add %in% link_ids(rv$board))) {
        stop("Expecting unique connection IDs for new connections.")
      }
    },
    priority = 1
  )

  observeEvent(
    val()$rm,
    {
      if (!is.character(val()$rm)) {
        stop("Expecting the `add` component of the `add_rm_conn` return ",
             "value to be a character vector.")
      }
    },
    once = TRUE
  )

  observeEvent(
    val()$rm,
    {
      if (!all(val()$rm %in% link_ids(rv$board))) {
        stop("Expecting all connection IDs to be removed to be known.")
      }
    },
    priority = 1
  )

  val
}

#' Add/remove block connections module
#'
#' Customizable logic for adding/removing connections between blocks on the
#' board.
#'
#' @param rv Reactive values object
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

      res <- reactiveVal()

      observeEvent(
        input$modify_links,
        {
          if ((is.null(link_updates$add) || !nrow(link_updates$add)) &&
              (is.null(link_updates$rm) || !length(link_updates$rm))) {

            showNotification(
              "No changes specified.",
              type = "warning"
            )

            return()
          }

          new <- tryCatch(
            modify_links(rv$board, link_updates$add, link_updates$rm),
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
            list(add = link_updates$add, rm = link_updates$rm)
          )

          link_updates$add <- NULL
          link_updates$rm <- NULL
          link_updates$curr <- board_links(new)

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


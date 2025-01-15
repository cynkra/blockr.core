#' Add/remove block links module
#'
#' Customizable logic for adding/removing links between blocks on the
#' board.
#'
#' @param rv Reactive values object
#'
#' @return A reactive value that evaualtes to `NULL` or a list with components
#' `add` and `rm`, where `add` is either `NULL` or a `data.frame` with columns
#' `id`, `from`, `tp` and `input` and `rm` is either `NULL` or a character
#' vector of link IDs.
#'
#' @rdname add_rm_link
#' @export
add_rm_link_server <- function(rv) {
  moduleServer(
    "add_rm_link",
    function(input, output, session) {

      observeEvent(input$links, {
        showModal(links_modal(session$ns))
      })

      upd <- reactiveValues(
        add = new_link(),
        rm = character(),
        curr = isolate(board_links(rv$board)),
        obs = list(),
        edit = NULL
      )

      observeEvent(board_links(rv$board), {
        upd$curr <- board_links(rv$board)
      })

      output$links <- DT::renderDT(
        {
          DT::datatable(
            dt_board_link(upd$curr, session$ns, rv),
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
        names(upd$curr),
        {
          ids <- names(upd$curr)

          upd <- create_dt_observers(setdiff(ids, names(upd$obs)), input, upd)
          upd <- destroy_dt_observers(setdiff(names(upd$obs), ids), upd)
        }
      )

      observeEvent(upd$edit, {

        row <- upd$edit$row
        col <- upd$edit$col

        if (!row %in% upd$row && row %in% board_link_ids(rv$board)) {
          upd$row <- c(upd$row, row)
        }

        new <- upd$curr[[row]]
        new[[col]] <- coal(upd$edit$val, "")

        upd$curr[[row]] <- new

        if (row %in% names(upd$add)) {
          upd$add[[row]] <- new
        } else {
          upd$add <- c(upd$add, new)
        }
      })

      observeEvent(input$add_link, {

        new <- new_link(from = "", to = "", input = "")

        upd$curr <- c(upd$curr, new)
        upd$add <- c(upd$add, new)
      })

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

      observeEvent(input$cancel_links, {
        removeModal()
        upd$curr <- board_links(rv$board)
      })

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
              add = if (length(upd$add)) upd$add,
              rm = if (length(upd$rm)) upd$rm
            )
          )

          upd$add <- NULL
          upd$rm <- NULL
          upd$curr <- board_links(new)

          removeModal()
        }
      )

      res
    }
  )
}

#' @param id Namespace ID
#' @param board The initial `board` object
#' @rdname add_rm_link
#' @export
add_rm_link_ui <- function(id, board) {

  ns <- NS(
    NS(id, "add_rm_link")
  )

  list(
    actionButton(
      ns("links"),
      "Edit links",
      icon = icon("table")
    )
  )
}

dt_board_link <- function(lnk, ns, rv) {

  ids <- list(choices = board_block_ids(rv$board))

  data.frame(
    From = chr_mply(dt_selectize, lapply(paste0(lnk$id, "_from"), ns),
                    lnk$from, MoreArgs = ids),
    To = chr_mply(dt_selectize, lapply(paste0(lnk$id, "_to"), ns),
                  lnk$to, MoreArgs = ids),
    Input = chr_mply(dt_selectize, lapply(paste0(lnk$id, "_input"), ns),
                     lnk$input, block_inputs(rv$board)[lnk$to])
  )
}

dt_selectize <- function(id, val, choices) {
  as.character(
    selectInput(id, label = "", choices = c("", choices), selected = val)
  )
}

create_dt_observers <- function(ids, input, update) {
  update$obs[ids] <- lapply(ids, create_dt_observers_for_id, input, update)
  update
}

create_dt_observers_for_id <- function(id, input, update) {

  obs <- set_names(nm = c("from", "to", "input"))

  lapply(obs, create_dt_observer, id, input, update)
}

create_dt_observer <- function(col, row, input, upd) {

  inp <- paste0(row, "_", col)

  observeEvent(
    input[[inp]],
    {
      val <- input[[inp]]
      if (val != upd$curr[[row]][[col]]) {
        upd$edit <- list(row = row, col = col, val = val)
      }
    },
    ignoreInit = TRUE
  )
}

destroy_dt_observers <- function(ids, update) {

  for (row in ids) {
    for (col in c("from", "to", "input")) {
      update$obs[[row]][[col]]$destroy()
    }
    update$obs[[row]] <- NULL
  }

  update
}

links_modal <- function(ns) {
  modalDialog(
    title = "Board links",
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

check_add_rm_link_val <- function(val, rv) {

  observeEvent(
    TRUE,
    {
      if (!is.reactive(val)) {
        stop("Expecting `add_rm_link` to return a reactive value.")
      }
    },
    once = TRUE
  )

  observeEvent(
    val(),
    {
      if (!is.list(val()) || !setequal(names(val()), c("add", "rm"))) {
        stop("Expecting the `add_rm_link` return value to evaluate to a list ",
             "with components `add` and `rm`.")
      }
    },
    once = TRUE
  )

  observeEvent(
    val()$add,
    {
      if (!is.data.frame(val()$add)) {
        stop("Expecting the `add` component of the `add_rm_link` return ",
             "value to be `NULL` or a `data.frame`.")
      }

      if (!all(c("id", "from", "to", "input") %in% colnames(val()$add))) {
        stop("Expecting the `add` component of the `add_rm_link` return ",
             "value to contain columns `id`, `from`, `to` and `input`.")
      }
    },
    once = TRUE
  )

  observeEvent(
    val()$add,
    {
      if (any(val()$add %in% board_link_ids(rv$board))) {
        stop("Expecting unique link IDs for new links.")
      }
    },
    priority = 1
  )

  observeEvent(
    val()$rm,
    {
      if (!is.character(val()$rm)) {
        stop("Expecting the `add` component of the `add_rm_link` return ",
             "value to be a character vector.")
      }
    },
    once = TRUE
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

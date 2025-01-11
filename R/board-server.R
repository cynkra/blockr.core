#' Board server
#'
#' Shiny server function for `board` objects.
#'
#' @param x Board
#'
#' @export
board_server <- function(x) {
  UseMethod("board_server")
}

#' @rdname board_server
#' @export
board_server.board <- function(x) {
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

      output$serialize <- downloadHandler(
        board_filename(rv$board),
        write_board_to_disk(rv$board, rv)
      )

      observeEvent(input$restore, {

        removeUI(
          paste0("#", board_id(rv$board), "_blocks > div")
        )

        rv$board <- from_json(
          readLines(input$restore$datapath)
        )

        insertUI(
          paste0("#", board_id(rv$board), "_blocks"),
          "afterBegin",
          block_cards(rv$board)
        )

        rv <- setup_blocks(rv)
      })

      observeEvent(input$add_block, {

        req(input$registry_select)

        blk <- create_block(input$registry_select)

        insertUI(
          paste0("#", board_id(rv$board), "_blocks"),
          "beforeEnd",
          block_card(blk, board_id(rv$board))
        )

        rv$board <- add_block(rv$board, blk)

        rv <- setup_block(blk, rv)
      })

      observe(
        updateSelectInput(
          session,
          inputId = "block_select",
          choices = block_ids(rv$board)
        )
      )

      observeEvent(input$rm_block, {

        req(input$block_select)

        removeUI(
          paste0("#", input$block_select, "_block")
        )

        rv <- destroy_block(input$block_select, rv)
      })

      output$links <- DT::renderDT(
        {
          DT::datatable(
            board_links(rv$board),
            options = list(pageLength = 5),
            editable = TRUE
          )
        },
        server = TRUE
      )

      observeEvent(input$links, {
        showModal(links_modal(session$ns))
      })

      link_updates <- reactiveValues(
        add = NULL,
        rm = NULL
      )

      observeEvent(input$links_cell_edit, {

        orig <- board_links(rv$board)
        temp <- orig[input$links_cell_edit$row, ]

        temp[, input$links_cell_edit$col] <- input$links_cell_edit$value

        link_updates$rm <- c(
          link_updates$rm,
          orig$id[input$links_cell_edit$row]
        )

        link_updates$add <- rbind(
          link_updates$add,
          temp
        )
      })

      observeEvent(input$modify_links, {
        rv <- tryCatch(
          {
            new <- modify_links(rv$board, link_updates$add, link_updates$rm)
            old <- board_links(rv$board)

            rv <- update_block_connections(
              rv,
              add = link_updates$add,
              rm = old[old$id %in% link_updates$rm, ]
            )

            link_updates$add <- NULL
            link_updates$rm <- NULL

            rv$board <- new

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

update_block_connections <- function(rv, add, rm) {

  for (i in seq_len(nrow(rm))) {
    rv <- do.call(destroy_connection, c(list(rv), rm[i, ]))
  }

  for (i in seq_len(nrow(add))) {
    rv <- do.call(setup_connection, c(list(rv), add[i, ]))
  }

  rv
}

links_modal <- function(ns) {
  modalDialog(
    title = "Board connections",
    DT::dataTableOutput(ns("links")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("modify_links"), "OK")
    )
  )
}

board_filename <- function(x) {
  function() {
    paste0(
      board_id(x), "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".json"
    )
  }
}

write_board_to_disk <- function(x, rv) {

  function(con) {

    blocks <- lapply(
      lapply(lapply(rv$blocks, `[[`, "server"), `[[`, "json"),
      reval
    )

    json <- jsonlite::prettify(
      to_json(x, blocks)
    )

    writeLines(json, con)
  }
}

setup_blocks <- function(rv) {

  stopifnot(
    is.reactivevalues(rv),
    setequal(names(rv), c("blocks", "inputs", "board", "links")),
    is_board(rv$board)
  )

  rv$blocks <- list()
  rv$inputs <- list()

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

  rv$inputs[[id]] <- NULL
  rv$blocks[[id]] <- NULL

  rv$board <- remove_blocks(rv$board, id)

  rv
}

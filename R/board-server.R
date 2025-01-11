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
        board = x
      )

      observeEvent(
        TRUE,
        {
          rv <- setup_blocks(rv)
        },
        priority = 100,
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

        link_updates$rm <- c(link_updates$rm, input$links_cell_edit$row)
        link_updates$add <- rbind(
          link_updates$add,
          orig[input$links_cell_edit$row, ]
        )
      })

      observeEvent(input$modify_links, {
        rv <- tryCatch(
          {
            new <- modify_links(rv$board, link_updates$add, link_updates$rm)
            old <- board_links(rv$board)

            update_block_links(rv, link_updates$add, old[link_updates$rm, ])

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

update_block_links <- function(rv, add, rm) {

  for (i in seq_len(nrow(rm))) {
    rv$inputs[[rm[i, "to"]]][[rm[i, "input"]]] <- function() list()
  }

  for (i in seq_len(nrow(add))) {
    rv$inputs[[add[i, "to"]]][[add[i, "input"]]] <-
      rv$blocks[[add[i, "from"]]]$server$result
  }

  rv
}

rm_block_link <- function(link, rv) {
  rv$inputs[[link[["to"]]]][[link[["input"]]]] <- function() list()
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
    setequal(names(rv), c("blocks", "inputs", "board")),
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

  links <- board_links(rv$board)

  if (block_arity(blk)) {

    hits <- links$to == id

    if (sum(hits)) {

      rv$inputs[[id]] <- lapply(
        set_names(links$from[hits], links$input[hits]),
        function(src) rv$blocks[[src]]$server$result
      )

    } else {

      rv$inputs[[id]] <- set_names(
        rep(list(function() list()), block_arity(blk)),
        block_inputs(blk)
      )
    }

  } else {

    rv$inputs[[id]] <- list()
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

destroy_block <- function(id, rv) {

  rv$inputs[[id]] <- NULL
  rv$blocks[[id]] <- NULL

  rv$board <- remove_blocks(rv$board, id)

  rv
}

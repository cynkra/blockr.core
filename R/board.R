# Utilities to populate the scoutbar with block
# registry information
# blk_cats <- sort(unique(chr_ply(available_blocks(), \(b) attr(b, "category"))))

# Create one page per block category
# blk_choices <- lapply(blk_cats, \(cat) {
#  scout_section(
#    label = cat,
#    .list = dropNulls(
#      unname(lapply(available_blocks(), \(choice) {
#        if (attr(choice, "category") == cat) {
#          scout_action(
#            id = attr(choice, "classes")[1],
#            label = paste0(attr(choice, "name"), " (", attr(choice, "package"), ")"),
#            description = attr(choice, "description")
#          )
#        }
#      })
#    ))
#  )
# })

blk_choices <- function() {
  scout_section(
    label = "Data",
    scout_action(
      id = attr(new_dataset_block(), "class")[1],
      label = "Dataset block",
      description = "Select data in package"
    )
  )
}

available_blocks <- function() {
  ctor <- new_dataset_block
  list(dataset_block = structure(
    ctor,
    title = attr(ctor(), "class")[1],
    description = "Select dataset from package"
  ))
}

#' @rdname board
#' @export
dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' @rdname board
#' @export
dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

#' @rdname board
#' @export
network_ui <- function(id) {
  ns <- NS(id)
  # Note: semantic ordering so that
  # elements can be used in the board in different places.
  list(
    action_bar = tagList(
      actionButton(
        ns("add_block"),
        "New block",
        icon = icon("circle-plus"),
        class = "btn-light"
      ),
      scoutbar(
        ns("scoutbar"),
        placeholder = "Search for a block",
       actions = blk_choices()
      )
    ),
    sidebar = div(
      class = "btn-group",
      role = "group",
      actionButton(ns("add_block_to"), "New block", icon = icon("circle-plus"), class = "btn-light"),
      actionButton(ns("remove"), "Remove block", icon = icon("trash"), class = "btn-light"),
    ),
    canvas = visNetworkOutput(ns("network"))
  )
}

#' @rdname board
#' @export
network_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      edges = data.frame(),
      nodes = data.frame(),
      new_block = NULL
    )

    output$network <- renderVisNetwork({
      # Initialized as empty, we'll update with the proxy
      visNetwork(
        data.frame(),
        data.frame(),
        height = "100vh",
        width = "100%"
      ) |>
        visOptions(
          # To get currently selected node
          nodesIdSelection = TRUE
        )
    })

    # Trigger add block
    observeEvent(input$add_block, {
      update_scoutbar(
        session,
        "scoutbar",
        revealScoutbar = TRUE
      )
    })

    # Adding a block, we need to instantiate the block
    # server module + update the vals$node so the graph is updated
    observeEvent(input$scoutbar, {
      # Construct block with empty defaults
      # TBD: maybe we want to provide more choices
      # but that would require more UI elements
      rv$new_block <- available_blocks()[[input$scoutbar]]()

      # Update node vals for the network rendering
      id <- attr(rv$new_block, "uid")
      tmp_network_data <- data.frame(
        id = id,
        label = attr(rv$new_block, "class")[1],
        # color = "red",
        title = NA,
        shape = "circle",
        stack = NA,
        icon.code = NA
      )
      if (nrow(rv$nodes) == 0) {
        rv$nodes <- tmp_network_data
      } else {
        rv$nodes <- rbind(
          rv$nodes,
          tmp_network_data
        )
      }

      visNetworkProxy(ns("network")) |>
        visUpdateNodes(rv$nodes)
    })

    # TBD: implement add_block_to -> add a block after the selected one
    # We need a contextual registry and update the scoutbar with relevant
    # choices. I think we can use the same scoutbar as for the classic
    # add block with all choices.

    # Remove a block
    # TBD: how do we handle multi block removal?
    observeEvent(input$remove, {
      row_to_remove <- which(rv$nodes$id == input$network_selected)
      rv$nodes <- rv$nodes[-row_to_remove, ]
      visNetworkProxy(ns("network")) |>
        visRemoveNodes(input$network_selected)
    })

    # TBD implement edge creation

    # The network module must return the edge representation
    # since they are created from the network and needed in other parts,
    # as well as the selected items such as current node and/or edge.
    return(
      list(
        edges = reactive(rv$edges),
        nodes = reactive(rv$nodes),
        selected_node = reactive(input$network_selected),
        added_block = reactive(rv$new_block),
        removed_block = eventReactive(input$remove, {
          # Contains the UID of the block module to remove from
          # the board
          input$network_selected
        })
      )
    )
  })
}

#' The board provides the main ui for blockr2
#'
#' The board is composed of 2 views pointing to the network module
#' or the dashboard module.
#' @param id Unique id.
#' @rdname board
#' @export
board_ui <- function(id) {
  ns <- NS(id)

  network_ui <- network_ui(ns("dag"))

  tagList(
    div(
      class = "d-flex justify-content-center align-items-center",
      div(
        class = "btn-group",
        role = "group",
        network_ui$action_bar,
        shinyWidgets::switchInput(
          ns("board_mode"),
          onLabel = "Network",
          offLabel = "Dashboard",
          value = TRUE,
          size = "mini"
        )
      )
    ),
    tabsetPanel(
      id = ns("board_tabs"),
      type = "hidden",
      tabPanelBody(
        "network_tab",
        layout_sidebar(
          sidebar = sidebar(
            id = ns("sidebar"),
            open = FALSE,
            width = 600,
            class = "rounded",
            bg = "white",
            position = "right",
            # Node module (ui filters + output)
            uiOutput(ns("node_ui")),
            network_ui$sidebar
          ),
          network_ui$canvas
        )
      ),
      tabPanelBody(
        "dashboard_tab",
        dashboard_ui(ns("dash"))
      )
    )
  )
}

#' @rdname board
#' @export
board_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # The board must know about the blocks
      rv <- reactiveValues(blocks = list())

      # DAG representation
      # network_out$connections: dataframe of connected node ids.
      network_out <- network_server("dag")

      # Dashboard mode
      dashboard_server("dash")

      # Switch between dashboard and network view
      # TBD: ideally we create a toggle input with 2 values
      observeEvent(input$board_mode, {
        tab <- if (input$board_mode) "network" else "dashboard"
        updateTabsetPanel(
          session,
          "board_tabs",
          selected = sprintf("%s_tab", tab)
        )
      })

      # Call block server module when node is added or removed
      observeEvent(network_out$added_block(), {
        blk <- network_out$added_block()
        rv$blocks[[attr(blk, "uid")]] <- list(
          # We need the block object to render the UI
          block = blk,
          # The server is the module from which we can
          # extract data, ...
          server = block_server(blk)
        )
      })
      
      observeEvent(network_out$removed_block(), {
        removeUI(sprintf("#%s", ns(network_out$removed_block())))
        rv$blocks[[network_out$removed_block()]] <- NULL
      })

      # When a node is selected, we need to display
      # sidebar with node UI module.
      # FIXME: at the moment, rv$blocks[[network_out$selected()]]$block
      # contains the block state when it was created, so UI inputs
      # are not synced with the server part.
      observeEvent({
        req(
          nchar(network_out$selected()) > 0,
          rv$blocks[[network_out$selected()]]
        )
      }, {
        # TBD: find a way to restore the block state
        # where is was on the server.
        blk <- rv$blocks[[network_out$selected()]]$block
        id <- ns(attr(blk, "uid"))
        removeUI(sprintf("#%s", id))
        insertUI(
          sprintf("#%s .sidebar-content", ns("sidebar")),
          where = "afterBegin",
          ui = block_ui(
            blk,
            id = ns(attr(blk, "uid"))
          )
        )
      })

      observeEvent(network_out$selected(),
        {
          bslib::toggle_sidebar(
            "sidebar",
            open = !is.null(network_out$selected()) && nchar(network_out$selected()) > 0
          )
        },
        ignoreNULL = FALSE
      )
    }
  )
}

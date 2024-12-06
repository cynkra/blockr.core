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
  visNetworkOutput(ns("network"))
}

#' @param rv_nodes Data frame of nodes. Each time new block is added,
#' a row is appended to this dataframe so the graph is updated via a proxy
#' (avoids to redraw the entire graph)
#' @rdname board
#' @export
network_server <- function(id, rv_nodes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv_edges <- reactiveVal(data.frame())

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

    # Add nodes when needed
    observeEvent(rv_nodes(), {
      visNetworkProxy(ns("network")) |>
        visUpdateNodes(rv_nodes())
    })

    # TBD implement edge creation

    # The network module must return the edge representation
    # since they are created from the network and needed in other parts,
    # as well as the selected items such as current node and/or edge.
    return(
      list(
        connections = rv_edges,
        selected_node = reactive(input$network_selected)
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
  tagList(
    div(
      class = "d-flex justify-content-center align-items-center",
      div(
        class = "btn-group",
        role = "group",
        actionButton(
          ns("add_block"),
          "New block",
          icon = icon("circle-plus"),
          class = "btn-light"
        ),
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
            # Remove and add next block are not part of the block module.
            div(
              class = "btn-group",
              role = "group",
              actionButton(ns("add_block_to"), "New block", icon = icon("circle-plus"), class = "btn-light"),
              actionButton(ns("remove"), "Remove block", icon = icon("trash"), class = "btn-light"),
            )
          ),
          scoutbar(
            ns("scoutbar"),
            placeholder = "Search for a block",
            actions = blk_choices()
          ),
          network_ui(ns("dag"))
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

      # The board must know about the blocks and nodes
      vals <- reactiveValues(
        blocks = list(),
        nodes = data.frame()
      )

      # DAG representation
      # network_out$connections: dataframe of connected node ids.
      network_out <- network_server(
        "dag",
        rv_nodes = reactive({
          req(nrow(vals$nodes) > 0)
          vals$nodes
        })
      )

      # Dashboard mode
      dashboard_server("dash")

      # Switch between dashboard and network view
      # TBD: ideally we create a toggle input with 2 values
      observeEvent(input$board_mode, {
        updateTabsetPanel(
          session,
          "board_tabs",
          selected = sprintf("%s_tab", input$board_mode)
        )
      })

      # Remove a block
      # We update reactives which will propagate to the
      # network module to update the visuals
      observeEvent(input$remove, {
        vals$blocks[[network_out$selected()]] <- NULL
        vals$nodes <- vals$nodes[-network_out$selected(), ]
      })

      # TBD: implement add_block_to -> add a block after the selected one

      # Adding a block, we need to instantiate the block
      # server module + update the vals$node so the graph is updated
      observeEvent(input$add_block, {
        update_scoutbar(
          session,
          "scoutbar",
          revealScoutbar = TRUE
        )
      })

      observeEvent(input$scoutbar, {
        # Construct block with empty defaults
        # TBD: maybe we want to provide more choices
        # but that would require more UI elements
        tmp_block <- available_blocks()[[input$scoutbar]]()

        # Update node vals for the network rendering
        tmp_network_data <- data.frame(
          id = attr(tmp_block, "uid"),
          label = attr(tmp_block, "class")[1],
          # color = "red",
          title = NA,
          shape = "circle",
          stack = NA,
          icon.code = NA
        )
        if (nrow(vals$nodes) == 0) {
          vals$nodes <- tmp_network_data
        } else {
          vals$nodes <- rbind(
            vals$nodes,
            tmp_network_data
          )
        }

        # TBD maybe we want to store the block object + its server returned values...
        vals$blocks[[attr(tmp_block, "uid")]] <- list(
          block = tmp_block,
          server = block_server(tmp_block)
        )
      })

      # When a node is selected, we need to display
      # sidebar with node UI module.
      # FIXME: with render UI, we lost input values choices
      # when a node get unselected. Is this a real issue?
      output$node_ui <- renderUI({
        req(nchar(network_out$selected()) > 0)
        block_ui(vals$blocks[[network_out$selected()]]$block)
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

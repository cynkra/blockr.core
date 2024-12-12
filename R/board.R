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

#' Add a node
#' 
#' Update dataframe for visNetwork graph
#' 
#' @param new New block to insert. Must be a valid
#' block registry entry
#' @param nodes dataframe representing network data.
#' @keywords internal
add_node <- function(new, nodes) {

  stopifnot(
    is_block(new),
    is.data.frame(nodes)
  )

  node_data <- data.frame(
    id = block_uid(new),
    label = attr(new, "class")[1],
    # color = "red",
    title = NA,
    shape = "circle",
    stack = NA,
    icon.code = NA
  )

  if (nrow(nodes) == 0) {
    node_data
  } else {
    rbind(
      nodes,
      node_data
    )
  }
}

#' Remove a node
#' 
#' Update dataframe for visNetwork graph
#' 
#' @param selected UID (character string) of node to remove.
#' @param nodes dataframe representing network data.
#' @keywords internal
remove_node <- function(selected, nodes) {
  stopifnot(
    is.data.frame(nodes),
    nrow(nodes) > 0,
    is.character(selected),
    nchar(selected) > 0
  )

  to_remove <- which(nodes$id == selected)
  if (length(to_remove) == 0) {
    stop(sprintf("Can't find node with id %s in the data", selected))
  }
  nodes[-to_remove, ]
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
      rv$new_block <- NULL
      update_scoutbar(
        session,
        "scoutbar",
        revealScoutbar = TRUE
      )
    })

    # Adding a block, we update the rv$nodes so the graph is updated
    observeEvent(input$scoutbar, {
      # Construct block with empty defaults
      # TBD: maybe we want to provide more choices
      # but that would require more UI elements
      rv$new_block <- available_blocks()[[input$scoutbar]]()

      # Update node vals for the network rendering
      rv$nodes <- add_node(rv$new_block, rv$nodes)

      visNetworkProxy(ns("network")) |>
        visUpdateNodes(rv$nodes)
    })

    # TBD handle node update: change of color due to block validity change ...

    # TBD: implement add_block_to -> add a block after the selected one
    # We need a contextual registry and update the scoutbar with relevant
    # choices. I think we can use the same scoutbar as for the classic
    # add block with all choices.

    # Remove a block
    # TBD: how do we handle multi block removal?
    observeEvent(input$remove, {
      rv$nodes <- remove_node(input$network_selected, rv$nodes)
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
      shinyWidgets::switchInput(
        ns("mode"),
        onStatus = "default",
        onLabel = icon("network-wired"),
        offLabel = icon("table-columns"),
        value = TRUE,
        size = "mini"
      ),
      div(
        class = "btn-group",
        role = "group",
        network_ui$action_bar
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
      observeEvent(input$mode, {
        tab <- if (input$mode) "network" else "dashboard"
        updateTabsetPanel(
          session,
          "board_tabs",
          selected = sprintf("%s_tab", tab)
        )
      })

      # Call block server module when node is added or removed
      observeEvent(network_out$added_block(), {
        blk <- network_out$added_block()
        rv$blocks[[block_uid(blk)]] <- list(
          # We need the block object to render the UI
          block = blk,
          # The server is the module from which we can
          # extract data, ...
          server = block_server(blk, data = list())
        )
      })
      
      observeEvent(network_out$removed_block(), {
        rv$blocks[[network_out$removed_block()]] <- NULL
        bslib::toggle_sidebar("sidebar", open = FALSE)
      })

      # When a node is selected, we need to display
      # sidebar with node UI module.
      output$node_ui <- renderUI({
        selected <- network_out$selected()
        req(
          nchar(selected) > 0,
          rv$blocks[[selected]]
        )
        tmp <- rv$blocks[[selected]]
        isolate({
          state <- lapply(tmp$server$state, \(el) {
            if (is.reactive(el)) el() else el
          })
          do.call(block_ui, c(list(x = tmp$block, id = id), state))
        })
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

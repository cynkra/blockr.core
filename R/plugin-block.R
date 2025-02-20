#' Edit block module
#'
#' Customizable logic for editing block attributes such as block title.
#'
#' @param id Namespace ID
#' @param x Static block
#' @param res Reactive result
#' @param ... Extra arguments passed from parent scope
#'
#' @return A list of [shiny::reactiveVal()] objects interpreted as block state
#' components.
#'
#' @rdname edit_block
#' @export
edit_block_server <- function(id, x, res, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      curr_block_name <- reactiveVal(block_name(x))

      observeEvent(
        input$block_name_in,
        {
          req(input$block_name_in)
          curr_block_name(input$block_name_in)
        }
      )

      output$block_name_out <- renderUI({
        list(
          bslib::tooltip(curr_block_name(), paste("Block ID: ", id)),
          bsicons::bs_icon("pencil-square")
        )
      })

      list(
        name = curr_block_name
      )
    }
  )
}

#' @rdname edit_block
#' @export
edit_block_ui <- function(x, id, ...) {

  tagList(
    bslib::card_header(
      bslib::popover(
        uiOutput(NS(id, "block_name_out"), inline = TRUE),
        title = "Provide a new block name",
        textInput(NS(id, "block_name_in"), NULL)
      )
    ),
    ...
  )
}

check_edit_block_val <- function(val) {

  observeEvent(
    TRUE,
    {
      if (!is.list(val)) {
        abort(
          "Expecting `edit_block` to return a list.",
          class = "edit_block_return_invalid"
        )
      }
    },
    once = TRUE
  )

  observeEvent(
    TRUE,
    {
      known <- block_base_attrs()
      if (!all(names(val) %in% known)) {
        abort(
          paste0(
            "Not expecting `edit_block` to return components ",
            paste_enum(setdiff(names(val), known)), "."
          ),
          class = "edit_block_return_invalid"
        )
      }
    },
    once = TRUE
  )

  val
}

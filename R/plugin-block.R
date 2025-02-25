#' Edit block module
#'
#' Customizable logic for editing block attributes such as block title.
#'
#' @param id Namespace ID
#' @param x Static block
#' @param res Reactive result
#' @param block_id Block ID
#' @param board Reactive values object containing board information
#' @param update Reactive value object to initiate board updates
#' @param ... Extra arguments passed from parent scope
#'
#' @return A list of [shiny::reactiveVal()] objects interpreted as block state
#' components.
#'
#' @rdname edit_block
#' @export
edit_block_server <- function(id, x, res, block_id, board, update, ...) {
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

      output$block_name_out <- renderUI(
        bslib::tooltip(curr_block_name(), paste("Block ID: ", block_id))
      )

      output$block_summary <- renderText(block_summary(x, res()))

      observeEvent(
        input$rm_block,
        update(list(blocks = list(rm = block_id)))
      )

      list(
        name = curr_block_name
      )
    }
  )
}

#' @rdname edit_block
#' @export
edit_block_ui <- function(x, id, ...) {

  opts <- bslib::popover(
    title = "Block options",
    bsicons::bs_icon("gear"),
    textInput(
      NS(id, "block_name_in"),
      "Block name"
    ),
    actionButton(
      NS(id, "rm_block"),
      "Remove block",
      icon = icon("circle-minus"),
      class = "btn-danger"
    )
  )

  tagList(
    bslib::card_header(
      class = "d-flex justify-content-between",
      uiOutput(NS(id, "block_name_out"), inline = TRUE),
      opts
    ),
    ...,
    bslib::card_footer(
      class = "text-center",
      uiOutput(NS(id, "block_summary"), inline = TRUE)
    )
  )
}

#' @param data Result data
#' @rdname edit_block
#' @export
block_summary <- function(x, data) {
  UseMethod("block_summary")
}

#' @rdname edit_block
#' @export
block_summary.block <- function(x, data) {
  paste0("&lt;", type_desc(data), "[", dim_desc(data), "]&gt;")
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

type_desc <- function(x) {
 if (is.object(x)) {
    class(x)[[1]]
  } else if (rlang::is_vector(x)) {
    typeof(x)
  }
}

dim_desc <- function(x) {
  dim <- coal(dim(x), vec_size(x))
  format_dim <- big_mark(dim)
  format_dim[is.na(dim)] <- "??"
  paste0(format_dim, collapse = paste0(" &#10005; "))
}

big_mark <- function(x) {

  if (identical(getOption("OutDec"), ",")) {
    mark <- "."
  } else {
    mark <- ","
  }

  ret <- formatC(x, big.mark = mark, format = "d",
                 preserve.width = "individual")

  ret[is.na(x)] <- "??"

  ret
}

#' Edit block module
#'
#' Customizable logic for editing block attributes such as block title.
#'
#' @param id Namespace ID
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
edit_block_server <- function(id, block_id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      initial_block <- isolate(
        board_blocks(board$board)[[block_id]]
      )

      curr_block_name <- reactiveVal(
        block_name(initial_block)
      )

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

      output$block_summary <- renderText(
        block_summary(
          initial_block,
          board$blocks[[block_id]]$server$result()
        )
      )

      observeEvent(
        input$rm_block,
        update(list(blocks = list(rm = block_id)))
      )

      observeEvent(
        input$opts,
        {
          if (is_leaf(block_id, board$board)) {
            insertUI(
              paste0(".popover.", session$ns(NULL), " > div.popover-body"),
              "beforeEnd",
              actionButton(
                session$ns("append_block"),
                "Append block",
                icon = icon("circle-plus"),
                class = "btn-success"
              )
            )
          }
        },
        ignoreInit = TRUE
      )

      observeEvent(
        input$append_block,
        showModal(
          append_block_modal(
            session$ns,
            is_block_in_stack(block_id, board$board)
          )
        )
      )

      observeEvent(
        input$registry_select,
        {
          sel <- input$registry_select

          if (!is_string(sel) || !sel %in% list_blocks()) {

            showNotification(
              "Please choose a valid block type.",
              type = "warning"
            )

            return()
          }

          updateSelectInput(
            session,
            "input_select",
            "Select data input",
            available_block_inputs(sel)
          )
        }
      )

      observeEvent(
        input$confirm_append,
        {
          sel <- input$registry_select
          bid <- input$block_id
          lid <- input$link_id
          inp <- input$input_select

          if (!validate_block_addition(sel, bid, board$board, session)) {
            return()
          }

          blk <- create_block(sel)

          add <- list(
            blocks = list(
              add = as_blocks(set_names(list(blk), bid))
            )
          )

          if (!validate_link_addition(lid, inp, blk, board$board, session)) {
            return()
          }

          add <- c(
            add,
            list(
              links = list(
                add = as_links(
                  set_names(list(new_link(block_id, bid, inp)), lid)
                )
              )
            )
          )

          if (isTRUE(input$add_to_stack)) {

            stacks <- board_stacks(board$board)

            stk <- is_block_in_stack(block_id, stacks)
            stk <- names(stk)[stk]

            add <- c(
              add,
              list(
                stacks = list(
                  add = as_stacks(lapply(stacks[stk], union, bid)),
                  rm = stk
                )
              )
            )
          }

          update(add)

          removeModal()
        }
      )

      observeEvent(
        input$cancel_append,
        removeModal()
      )

      list(
        name = curr_block_name
      )
    }
  )
}

#' @param x Block
#' @rdname edit_block
#' @export
edit_block_ui <- function(x, id, ...) {

  tagList(
    bslib::card_header(
      class = "d-flex justify-content-between",
      uiOutput(NS(id, "block_name_out"), inline = TRUE),
      bslib::popover(
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
        ),
        title = "Block options",
        id = NS(id, "opts"),
        options = list(customClass = id)
      )
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

append_block_modal <- function(ns, stack_checkbox = FALSE) {
  modalDialog(
    title = "Append block",
    div(
      selectInput(
        ns("registry_select"),
        "Select block from registry",
        choices = list_suitable_blocks()
      ),
      textInput(
        inputId = ns("block_id"),
        label = "Block ID",
        value = rand_names(),
        placeholder = "Enter a block ID."
      ),
      selectInput(
        ns("input_select"),
        "Select data input",
        choices = ""
      ),
      textInput(
        inputId = ns("link_id"),
        label = "Link ID",
        value = rand_names(),
        placeholder = "Enter a link ID."
      ),
      if (stack_checkbox) {
        bslib::input_switch(
          id = ns("add_to_stack"),
          label = "Add to stack",
          value = TRUE
        )
      }
    ),
    footer = tagList(
      actionButton(ns("cancel_append"), "Cancel", class = "btn-danger"),
      actionButton(ns("confirm_append"), "OK", class = "btn-success")
    )
  )
}

list_suitable_blocks <- function() {
  opts <- list_blocks()
  arity <- int_ply(lapply(opts, create_block), block_arity)
  opts[is.na(arity) | arity >= 1L]
}

available_block_inputs <- function(block) {

  blk <- create_block(block)
  inp <- block_inputs(blk)

  if (is.na(block_arity(blk))) {
    inp <- c(inp, as.character(seq_len(length(inp + 1L))))
  }

  inp
}

validate_link_addition <- function(id, input, block, board, session) {

  if (nchar(id) == 0L || !is_string(id)) {

    showNotification(
      "Please choose a valid link ID.",
      type = "warning",
      session = session
    )

    return(FALSE)
  }

  if (id %in% board_link_ids(board)) {

    showNotification(
      "Please choose a unique link ID.",
      type = "warning",
      session = session
    )

    return(FALSE)
  }

  inps <- block_inputs(block)

  if (is.na(block_arity(block))) {
    inps <- c(inps, as.character(seq_len(length(inps + 1L))))
  }

  if (!is_string(input) || !input %in% inps) {

    showNotification(
      "Please choose a valid block data input.",
      type = "warning"
    )

    return(FALSE)
  }

  TRUE
}

is_block_in_stack <- function(block, stacks) {

  is_el <- function(stk, el) is.element(el, stk)

  if (is_board(stacks)) {
    stacks <- board_stacks(stacks)
  }

  lgl_ply(stacks, is_el, block, use_names = TRUE)
}

is_block_in_stacks <- function(block, stacks) {
  any(is_block_in_stack(block, stacks))
}

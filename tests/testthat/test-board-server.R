test_that("board server", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()
      expect_equal(rv$blocks$b$server$result(), iris)
    },
    args = list(x = board)
  )

  empty <- new_board()

  testServer(
    get_s3_method("board_server", empty),
    {
      expect_length(board_blocks(x), 0L)
      expect_length(board_blocks(rv$board), 0L)

      expect_null(board_update())

      board_update(
        list(
          blocks = list(add = as_blocks(new_dataset_block()))
        )
      )

      session$flushReact()

      expect_length(board_blocks(x), 0L)
      expect_length(board_blocks(rv$board), 1L)

      expect_null(board_update())

      board_update(
        list(
          blocks = list(add = as_blocks(new_subset_block()))
        )
      )

      session$flushReact()

      expect_length(board_blocks(x), 0L)
      expect_length(board_blocks(rv$board), 2L)

      expect_null(board_update())

      board_update(
        list(
          blocks = list(rm = board_block_ids(rv$board))
        )
      )

      session$flushReact()

      expect_length(board_blocks(x), 0L)
      expect_length(board_blocks(rv$board), 0L)

      expect_null(board_update())
    },
    args = list(
      x = empty,
      plugins = list(
        manage_blocks(server = add_rm_block_server, ui = add_rm_block_ui)
      )
    )
  )

  testServer(
    get_s3_method("board_server", empty),
    {
      expect_length(board_blocks(x), 0L)
    },
    args = list(
      x = empty,
      callbacks = function(rv, ...) {
        expect_length(board_blocks(rv$board), 0L)
        NULL
      }
    )
  )

  test_xtra_args <- function(id, board, plugin_a, plugin_b) {
    moduleServer(
      id,
      function(input, output, session) {

        parent <- reactiveVal()

        board_server(
          "board",
          board,
          list(
            preserve_board(server = plugin_a, ui = NULL),
            manage_blocks(server = plugin_b, ui = NULL)
          ),
          parent = parent
        )
      }
    )
  }

  testServer(
    test_xtra_args,
    session$flushReact(),
    args = list(
      board = empty,
      plugin_a = function(id, rv, update, parent) {
        moduleServer(
          id,
          function(input, output, session) {
            observeEvent(TRUE, parent(1L), once = TRUE)
            reactiveVal()
          }
        )
      },
      plugin_b = function(id, rv, update, parent) {
        moduleServer(
          id,
          function(input, output, session) {
            observeEvent(
              parent(),
              expect_identical(parent(), 1L)
            )
            NULL
          }
        )
      }
    )
  )
})

test_that("update validation", {

  with_mock_session(
    {
      expect_error(
        validate_board_update(list(), list()),
        class = "board_update_object_invalid"
      )

      expect_error(
        validate_board_update(reactiveVal("a"), list()),
        class = "board_update_type_invalid"
      )

      expect_error(
        validate_board_update(
          reactiveVal(list(block = list(add = "a"))),
          list()
        ),
        class = "board_update_components_invalid"
      )
    }
  )
})

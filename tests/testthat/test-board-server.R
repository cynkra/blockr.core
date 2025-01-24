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
      expect_null(notifications())
    },
    args = list(x = board)
  )

  empty <- new_board()

  testServer(
    get_s3_method("board_server", empty),
    {
      expect_length(board_blocks(x), 0L)
      expect_length(board_blocks(rv$board), 0L)

      blocks$add <- as_blocks(new_dataset_block())
      session$flushReact()

      expect_length(board_blocks(x), 0L)
      expect_length(board_blocks(rv$board), 1L)

      blocks$add <- as_blocks(new_subset_block())
      session$flushReact()

      expect_length(board_blocks(x), 0L)
      expect_length(board_blocks(rv$board), 2L)

      blocks$rm <- board_block_ids(rv$board)
      session$flushReact()

      expect_length(board_blocks(x), 0L)
      expect_length(board_blocks(rv$board), 0L)
    },
    args = list(
      x = empty,
      plugins = list(manage_blocks = add_rm_block_server)
    )
  )

  testServer(
    get_s3_method("board_server", empty),
    {
      expect_length(board_blocks(x), 0L)
    },
    args = list(
      x = empty,
      callbacks = function(rv) {
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
          list(preseve_board = plugin_a, manage_blocks = plugin_b),
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
      plugin_a = function(id, rv, parent) {
        moduleServer(
          id,
          function(input, output, session) {
            observeEvent(TRUE, parent(1L), once = TRUE)
            reactiveVal()
          }
        )
      },
      plugin_b = function(id, rv, parent) {
        moduleServer(
          id,
          function(input, output, session) {
            observeEvent(
              parent(),
              expect_identical(parent(), 1L)
            )
            reactiveValues(add = NULL, rm = NULL)
          }
        )
      }
    )
  )
})

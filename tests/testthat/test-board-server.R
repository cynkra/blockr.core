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
})

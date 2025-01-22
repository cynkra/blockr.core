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
})

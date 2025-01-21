test_that("block server", {

  blk <- new_dataset_block("iris")

  testServer(
    get_s3_method("block_server", blk),
    {
      expect_true(all(state_check()))
      session$flushReact()
      expect_equal(session$returned$result(), iris)
    },
    args = list(x = blk, data = list())
  )
})

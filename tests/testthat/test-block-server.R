test_that("block server", {

  blk <- new_dataset_block("iris")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      expect_equal(session$returned$result(), iris)

      session$makeScope("expr")$setInputs(dataset = "mtcars")
      expect_equal(session$returned$result(), mtcars)
    },
    args = list(x = blk, data = list())
  )
})

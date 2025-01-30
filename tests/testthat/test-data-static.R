test_that("static block constructor", {

  blk <- new_static_block(datasets::mtcars)

  expect_s3_class(blk, "static_block")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_identical(
        session$returned$result(),
        datasets::mtcars
      )
    },
    args = list(x = blk)
  )
})

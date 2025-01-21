test_that("subset block constructor", {

  blk <- new_subset_block()

  expect_s3_class(blk, c("subset_block", "data_block", "block"))

  testServer(
    block_expr_server(blk),
    {
      expect_equal(sub(), "")
      expect_equal(sel(), "")

      expect_equal(session$returned$state$subset(), "")
      expect_equal(session$returned$state$select(), "")
    },
    args = list(data = mtcars)
  )
})

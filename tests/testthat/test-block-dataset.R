test_that("dataset block constructor", {

  blk <- new_dataset_block("iris")

  expect_s3_class(blk, c("dataset_block", "data_block", "block"))

  testServer(block_expr_server(blk), {
    expect_equal(dat(), "iris")
    session$setInputs(dataset = "mtcars")
    expect_equal(dat(), "mtcars")
  })
})

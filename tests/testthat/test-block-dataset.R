test_that("dataset block constructor", {
  blk <- new_dataset_block("iris")
  expect_s3_class(blk, c("dataset_block", "data_block", "block"))
})

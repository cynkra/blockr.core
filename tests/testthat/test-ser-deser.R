test_that("serialization", {

  orig <- new_dataset_block("iris", "datasets")

  json <- to_json(
    orig,
    list(dataset = "iris", package = "datasets")
  )

  blk <- from_json(json)

  expect_equal(orig, blk, ignore_function_env = TRUE)
})

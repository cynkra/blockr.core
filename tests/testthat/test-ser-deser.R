test_that("serialization", {

  orig <- new_dataset_block("iris", "datasets")
  json <- to_json(orig)

  expect_equal(orig, from_json(json), ignore_function_env = TRUE)

  id <- "abc"

  json <- to_json(
    new_dataset_block(uid = id),
    list(dataset = "iris", package = "datasets")
  )

  expect_equal(
    new_dataset_block("iris", "datasets", uid = id),
    from_json(json),
    ignore_function_env = TRUE
  )
})

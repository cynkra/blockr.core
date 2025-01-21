test_that("serialization", {

  orig <- new_dataset_block("iris", "datasets")
  json <- to_json(orig)

  expect_equal(orig, from_json(json), ignore_function_env = TRUE)

  json <- to_json(
    new_dataset_block(),
    list(dataset = "iris", package = "datasets")
  )

  expect_equal(
    new_dataset_block("iris", "datasets"),
    from_json(json),
    ignore_function_env = TRUE
  )

  orig <- c(
    a = new_dataset_block(),
    b = new_subset_block()
  )

  expect_equal(
    orig,
    from_json(to_json(orig)),
    ignore_function_env = TRUE
  )

  orig <- blocks(
    a = new_dataset_block("iris", "datasets")
  )

  json <- to_json(
    blocks(a = new_dataset_block()),
    list(
      a = list(dataset = "iris", package = "datasets")
    )
  )

  expect_equal(orig, from_json(json), ignore_function_env = TRUE)

  orig <- links(from = "a", to = "b")

  expect_equal(orig, from_json(to_json(orig)))

  orig <- new_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  expect_equal(
    orig,
    from_json(to_json(orig)),
    ignore_function_env = TRUE
  )
})

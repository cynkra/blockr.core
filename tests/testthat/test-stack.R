test_that("stacks", {

  x <- new_stack(letters[1:5])
  y <- new_stack(letters[6:8])

  expect_length(c(x, y), 8L)

  expect_error(c(x, x), "Stack blocks have to be unique")

  z <- new_stack("a")

  expect_length(setdiff(x, z), 4L)
  expect_s3_class(setdiff(x, z), "stack")
  expect_identical(setdiff(x, z), setdiff(x, "a"))

  expect_length(setdiff(letters[1:5], z), 4L)
  expect_s3_class(setdiff(letters[1:5], z), "stack")

  expect_length(union(x, z), length(x))
})

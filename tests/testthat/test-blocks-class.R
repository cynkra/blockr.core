test_that("blocks utils", {

  a <- new_dataset_block()
  b <- new_subset_block()

  ab <- c(a, b)

  expect_s3_class(ab, "blocks")
  expect_length(ab, 2L)
  expect_named(ab)

  names(ab) <- c("a", "b")

  expect_named(ab, c("a", "b"))

  names(ab)[2] <- "c"

  expect_named(ab, c("a", "c"))
})

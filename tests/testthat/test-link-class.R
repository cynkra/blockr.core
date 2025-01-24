test_that("link class", {

  x <- new_link("a", "b")

  expect_s3_class(x, "link")
  expect_snapshot(print(x))
})

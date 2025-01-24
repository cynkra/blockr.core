test_that("links class", {

  x <- new_link("a", "b")
  y <- new_link("b", "c")
  z <- new_link("c", "a")

  lnk <- links(x, y)

  expect_s3_class(lnk, "links")
  expect_true(is_links(lnk))
  expect_false(is_links("lnk"))
  expect_named(lnk)
  expect_named(links(x = x, y = y), c("x", "y"))

  expect_identical(lnk, as_links(lnk))
  expect_identical(lnk, as_links(as.list(lnk)))

  expect_error(links(x = x, x = y), class = "links_names_unique_invalid")
  expect_error(c(lnk, z), class = "links_acyclic_invalid")

  expect_snapshot(print(links(a = x, b = y)))
})

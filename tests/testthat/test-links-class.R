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

  z <- new_link("c", "d")

  lnks <- c(x, y)

  expect_s3_class(lnks, "links")

  expect_s3_class(c(lnks, z), "links")
  expect_s3_class(c(z, lnks), "links")

  lnks <- c(x = x, y = y)

  expect_s3_class(lnks, "links")
  expect_named(lnks, c("x", "y"))

  lnks[1] <- list(from = "a", to = "d")
  expect_identical(lnks[["x"]], as_link(list(from = "a", to = "d")))

  lnks[1] <- new_link(from = "a", to = "d")
  expect_identical(lnks[["x"]], as_link(list(from = "a", to = "d")))

  lnks[1] <- links(x = new_link(from = "a", to = "d"))
  expect_identical(lnks[["x"]], as_link(list(from = "a", to = "d")))

  expect_error(
    lnks[1] <- links(y = new_link(from = "a", to = "d")),
    class = "links_assignment_name_invalid"
  )

  lnks[1] <- data.frame(from = "a", to = "d")
  expect_identical(lnks[["x"]], as_link(list(from = "a", to = "d")))
})

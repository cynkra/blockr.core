test_that("exprs_to_lang", {

  eval_exprs <- function(exprs) {
    eval(exprs_to_lang(exprs), list())
  }

  deparse_exprs <- function(exprs) {
    deparse(call("local", exprs_to_lang(exprs)))
  }

  expected_eval <- 3
  expected_dparse <- c(
    "local({",
    "    x <- 1",
    "    y <- 2",
    "    x + y", "})"
  )

  a <- quote(
    {
      x <- 1
      y <- 2
      x + y
    }
  )

  expect_identical(eval_exprs(a), expected_eval)
  expect_identical(deparse_exprs(a), expected_dparse)

  b <- parse(
    text = "
      x <- 1
      y <- 2
      x + y
    "
  )

  expect_identical(eval_exprs(b), expected_eval)
  expect_identical(deparse_exprs(b), expected_dparse)

  c <- call("{", quote(x <- 1), quote(y <- 2), quote(x + y))

  expect_identical(eval_exprs(c), expected_eval)
  expect_identical(deparse_exprs(c), expected_dparse)

  d <- list(quote(x <- 1), quote(y <- 2), quote(x + y))

  expect_identical(eval_exprs(d), expected_eval)
  expect_identical(deparse_exprs(d), expected_dparse)

  e <- expression(x <- 1, y <- 2, x + y)

  expect_identical(eval_exprs(e), expected_eval)
  expect_identical(deparse_exprs(e), expected_dparse)

  f <- quote(1 + 2)

  expect_identical(eval_exprs(f), expected_eval)
  expect_identical(deparse_exprs(f), "local(1 + 2)")

  g <- quote(sum(1, 2))

  expect_identical(eval_exprs(g), expected_eval)
  expect_identical(deparse_exprs(g), "local(sum(1, 2))")

  f <- 3

  expect_identical(eval_exprs(f), expected_eval)
  expect_identical(deparse_exprs(f), "local(3)")
})

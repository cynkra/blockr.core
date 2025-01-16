test_that("zero length", {

  x <- list()

  expect_true(all_zero_len(x))
  expect_null(filter_all_zero_len(x))

  x <- list(1)

  expect_false(all_zero_len(x))
  expect_identical(x, filter_all_zero_len(x))

  x <- list(
    list(
      a = list(),
      b = NULL,
      c = character()
    )
  )

  expect_true(all_zero_len(x))
  expect_null(filter_all_zero_len(x))

  x <- list(
    list(
      a = list("foo"),
      b = NULL,
      c = character()
    )
  )

  expect_false(all_zero_len(x))
  expect_identical(
    filter_all_zero_len(x),
    list(list(a = list("foo")))
  )

  x <- list(
    list(
      a = list(
        a = NULL,
        b = list()
      ),
      b = NULL,
      c = character()
    )
  )

  expect_true(all_zero_len(x))
  expect_null(filter_all_zero_len(x))

  x <- list(
    list(
      a = list(
        a = NULL,
        b = list()
      ),
      b = "NULL",
      c = character()
    )
  )

  expect_false(all_zero_len(x))
  expect_identical(
    filter_all_zero_len(x),
    list(list(b = "NULL"))
  )

  x <- list(
    list(
      a = list(
        a = NULL,
        b = list(
          a = character(),
          b = list()
        )
      ),
      b = NULL,
      c = character()
    )
  )

  expect_true(all_zero_len(x))
  expect_null(filter_all_zero_len(x))
})

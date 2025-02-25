test_that("assertions", {

  expect_true(is_bool(TRUE))
  expect_true(is_bool(FALSE))

  expect_false(is_bool(0L))
  expect_false(is_bool(1))
  expect_false(is_bool(NA))
  expect_false(is_bool(list()))
  expect_false(is_bool(NULL))
  expect_false(is_bool(c(TRUE, FALSE)))

  expect_true(is_count(1))
  expect_true(is_count(1L))

  expect_false(is_count(1.5))
  expect_false(is_count(-1L))
  expect_false(is_count(1L:3L))
  expect_false(is_count(0L, allow_zero = FALSE))

  expect_true(is_number(1))
  expect_true(is_number(1L))
  expect_true(is_number(1.5))
  expect_true(is_number(-1L))

  expect_false(is_number(1:3))
  expect_false(is_number(NaN))
  expect_false(is_number(Inf))
  expect_false(is_number(NA_integer_))
})

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

test_that("*_xtr", {

  x <- list(
    list(
      a = "1",
      b = "2"
    ),
    list(
      a = "3",
      b = "4"
    )
  )

  expect_identical(lst_xtr(x, "a"), list("1", "3"))

  x <- list(
    list(
      a = list(a = "1", b = "2"),
      b = list(a = "3", b = "4")
    ),
    list(
      a = list(a = "5", b = "6"),
      b = list(a = "7", b = "8")
    )
  )

  expect_identical(lst_xtr(x, "a", "a"), list("1", "5"))
})

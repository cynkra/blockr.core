test_that("ply", {

  dat <- replicate(5, sample(letters, 10), simplify = FALSE)

  expect_length(
    chr_ply(dat, paste0, collapse = ""),
    5
  )

  expect_length(
    lgl_ply(dat, `==`, "a", length = 10L),
    5 * 10
  )

  expect_length(
    int_ply(dat, match, "a", length = 10L),
    5 * 10
  )

  dat <- replicate(5, sample(seq(0, 1, by = 0.1), 10), simplify = FALSE)

  expect_length(dbl_ply(dat, sum), 5)

  expect_length(chr_mply(paste, letters, LETTERS), length(letters))

  a <- sample(c(T, F), 10, replace = TRUE)
  b <- sample(c(T, F), 10, replace = TRUE)

  expect_length(lgl_mply(`|`, a, b), 10)

  expect_length(int_mply(`+`, seq.int(10), seq.int(10)), 10)
  expect_length(dbl_mply(`+`, seq(0, 1, by = 0.1), seq(0, 1, by = 0.1)), 11)

  expect_length(
    chr_xtr(replicate(5, sample(letters, 10), simplify = FALSE), 3L),
    5
  )

  expect_length(
    lgl_xtr(replicate(5, rep(TRUE, 5), simplify = FALSE), 3L),
    5
  )

  expect_length(
    int_xtr(replicate(5, rep(1L, 5), simplify = FALSE), 3L),
    5
  )

  expect_length(
    dbl_xtr(replicate(5, rep(1.5, 5), simplify = FALSE), 3L),
    5
  )

  expect_length(
    lst_xtr(replicate(5, rep(1.5, 5), simplify = FALSE), 3L),
    5
  )

  expect_named(
    map(paste, set_names(nm = letters), set_names(LETTERS, letters)),
    NULL
  )
})

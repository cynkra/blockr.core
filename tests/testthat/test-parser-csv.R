test_that("csv block constructor", {

  file <- withr::local_tempfile()
  utils::write.csv(datasets::BOD, file, row.names = FALSE)

  blk <- new_csv_block()

  expect_s3_class(blk, "csv_block")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_equal(
        session$returned$result(),
        datasets::BOD,
        ignore_attr = TRUE
      )
    },
    args = list(x = blk, data = list(file = function() file))
  )
})

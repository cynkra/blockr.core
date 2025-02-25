test_that("dummy parser block ui test", {
  expect_s3_class(block_ui("parser_block", new_csv_block()), "shiny.tag.list")
})

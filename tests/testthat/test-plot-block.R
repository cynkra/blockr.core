test_that("dummy parser block ui test", {
  expect_s3_class(block_ui("plot_block", new_scatter_block()), "shiny.tag.list")
})

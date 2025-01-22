test_that("dummy board ui test", {

  expect_s3_class(board_ui("board", new_board()), "shiny.tag.list")

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  expect_s3_class(board_ui("board", board), "shiny.tag.list")
})

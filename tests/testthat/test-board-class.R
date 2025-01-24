test_that("block constructor", {

  expect_s3_class(new_board(new_dataset_block()), "board")

  board <- new_board(
    list(
      d = blockr.dplyr::new_join_block(),
      a = new_dataset_block(),
      c = blockr.dplyr::new_select_block(),
      e = blockr.dplyr::new_select_block(),
      b = new_dataset_block()
    ),
    data.frame(
      id = c("ad", "cd", "bc", "de"),
      from = c("a", "c", "b", "d"),
      to = c("d", "d", "c", "e"),
      input = c("x", "y", "", "")
    )
  )

  expect_s3_class(board, "board")
  expect_snapshot(print(board))

  expect_error(
    new_board(
      list(
        a = new_dataset_block(),
        b = blockr.dplyr::new_select_block()
      ),
      new_link("a", "b", "foo")
    ),
    "expects inputs"
  )

  expect_error(
    new_board(
      list(
        a = new_dataset_block(),
        b = blockr.dplyr::new_select_block()
      ),
      data.frame(from = "a", to = "b", input = "foo")
    ),
    "Block b expects .+ but received .+"
  )
})

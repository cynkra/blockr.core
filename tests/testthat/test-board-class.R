test_that("block constructor", {

  expect_s3_class(new_board(new_dataset_block()), "board")

  board <- new_board(
    list(
      blockr.dplyr::new_join_block(uid = "d"),
      new_dataset_block(uid = "a"),
      blockr.dplyr::new_select_block(uid = "c"),
      blockr.dplyr::new_select_block(uid = "e"),
      new_dataset_block(uid = "b")
    ),
    data.frame(
      from = c("a", "c", "b", "d"),
      to = c("d", "d", "c", "e"),
      input = c("x", "y", "", "")
    )
  )

  expect_s3_class(board, "board")

  expect_error(
    new_board(
      list(
        new_dataset_block(uid = "a"),
        blockr.dplyr::new_select_block(uid = "b")
      ),
      new_link("a", "b", "foo")
    ),
    "expects inputs"
  )

  expect_error(
    new_board(
      list(
        new_dataset_block(uid = "a"),
        blockr.dplyr::new_select_block(uid = "b")
      ),
      data.frame(from = "a", to = "b", input = "foo")
    ),
    "Block b expects inputs data but received foo"
  )
})

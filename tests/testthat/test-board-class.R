test_that("block constructor", {

  expect_s3_class(new_board(new_dataset_block()), "board")

  board <- new_board(
    list(
      d = new_merge_block(),
      a = new_dataset_block(),
      c = new_subset_block(),
      e = new_subset_block(),
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

  sorted <- sort(board)

  expect_true(
    match("a", board_block_ids(sorted)) < match("d", board_block_ids(sorted))
  )

  expect_true(
    match("b", board_block_ids(sorted)) < match("d", board_block_ids(sorted))
  )

  expect_true(
    match("b", board_block_ids(sorted)) < match("c", board_block_ids(sorted))
  )

  expect_true(
    match("c", board_block_ids(sorted)) < match("d", board_block_ids(sorted))
  )

  expect_error(
    new_board(
      list(
        a = new_dataset_block(),
        b = new_subset_block()
      ),
      new_link("a", "b", "foo")
    ),
    "expects inputs"
  )

  expect_error(
    new_board(
      list(
        a = new_dataset_block(),
        b = new_subset_block()
      ),
      data.frame(from = "a", to = "b", input = "foo")
    ),
    "Block b expects .+ but received .+"
  )
})

test_that("board app", {

  skip_on_cran()

  app_path <- pkg_file("examples", "board", "app.R")

  app <- shinytest2::AppDriver$new(
    app_path,
    name = "board",
    seed = 42
  )

  app$click("my_board-manage_blocks-add_block")
  app$set_inputs(`my_board-manage_blocks-registry_select` = "dataset_block")
  app$set_inputs(`my_board-manage_blocks-block_id` = "a")
  app$click("my_board-manage_blocks-confirm_add")

  app$wait_for_idle()

  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$click("my_board-manage_blocks-add_block")
  app$set_inputs(`my_board-manage_blocks-registry_select` = "dataset_block")
  app$set_inputs(`my_board-manage_blocks-block_id` = "b")
  app$click("my_board-manage_blocks-confirm_add")

  app$wait_for_idle()

  app$set_inputs(`my_board-b-expr-dataset` = "ChickWeight")

  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$click("my_board-manage_blocks-add_block")
  app$set_inputs(`my_board-manage_blocks-registry_select` = "merge_block")
  app$set_inputs(`my_board-manage_blocks-block_id` = "c")
  app$click("my_board-manage_blocks-confirm_add")

  app$wait_for_idle()

  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$click("my_board-manage_links-links_mod")

  app$set_inputs(`my_board-manage_links-new_link_id` = "ac")
  app$click("my_board-manage_links-add_link")

  app$wait_for_idle()

  app$set_inputs(`my_board-manage_links-ac_from` = "a")
  app$set_inputs(`my_board-manage_links-ac_to` = "c")
  app$set_inputs(`my_board-manage_links-ac_input` = "x")

  app$wait_for_idle()

  app$set_inputs(`my_board-manage_links-new_link_id` = "bc")
  app$click("my_board-manage_links-add_link")

  app$wait_for_idle()

  app$set_inputs(`my_board-manage_links-bc_from` = "b")
  app$set_inputs(`my_board-manage_links-bc_to` = "c")
  app$set_inputs(`my_board-manage_links-bc_input` = "y")

  app$wait_for_idle()

  app$click("my_board-manage_links-modify_links")

  app$set_inputs(`my_board-c-expr-by` = "Time")

  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$click("my_board-manage_blocks-rm_block")

  app$wait_for_idle()

  app$set_inputs(`my_board-manage_blocks-block_select` = "a")
  app$click("my_board-manage_blocks-confirm_rm")

  app$wait_for_idle()

  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$stop()
})

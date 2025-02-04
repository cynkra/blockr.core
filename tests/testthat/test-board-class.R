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

  app_path <- system.file(
    "examples", "board", "app.R",
    package = "blockr.core"
  )

  app <- shinytest2::AppDriver$new(
    app_path,
    name = "board",
    seed = 42
  )

  app$click("my_board-manage_blocks-add_block")
  app$set_inputs(`my_board-manage_blocks-registry_select` = "dataset_block")
  app$set_inputs(`my_board-manage_blocks-block_id` = "a")
  app$click("my_board-manage_blocks-confirm_add")

  app$wait_for_idle(duration = 500)

  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$click("my_board-manage_blocks-add_block")
  app$set_inputs(`my_board-manage_blocks-registry_select` = "dataset_block")
  app$set_inputs(`my_board-manage_blocks-block_id` = "b")
  app$click("my_board-manage_blocks-confirm_add")

  app$wait_for_idle(duration = 500)

  app$set_inputs(`my_board-b-expr-dataset` = "ChickWeight")

  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$click("my_board-manage_blocks-add_block")
  app$set_inputs(`my_board-manage_blocks-registry_select` = "merge_block")
  app$set_inputs(`my_board-manage_blocks-block_id` = "c")
  app$click("my_board-manage_blocks-confirm_add")

  app$wait_for_idle(duration = 500)

  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$click("my_board-manage_links-links")

  app$click("my_board-manage_links-add_link")
  app$wait_for_value(input = "my_board-manage_links-edbxrcxwquzrffb_id")
  app$set_inputs(`my_board-manage_links-edbxrcxwquzrffb_from` = "a")
  app$set_inputs(`my_board-manage_links-edbxrcxwquzrffb_to` = "c")
  app$set_inputs(`my_board-manage_links-edbxrcxwquzrffb_input` = "x")

  app$click("my_board-manage_links-add_link")
  app$wait_for_value(input = "my_board-manage_links-tcvubwfzjheaqgd_id")
  app$set_inputs(`my_board-manage_links-tcvubwfzjheaqgd_from` = "b")
  app$set_inputs(`my_board-manage_links-tcvubwfzjheaqgd_to` = "c")
  app$set_inputs(`my_board-manage_links-tcvubwfzjheaqgd_input` = "y")

  app$wait_for_idle(duration = 500)

  app$click("my_board-manage_links-modify_links")

  app$set_inputs(`my_board-c-expr-by` = "Time")

  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$stop()
})

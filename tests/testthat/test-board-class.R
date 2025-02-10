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

  links_input_ids <- function(curr = character()) {

    res <- grep(
      "my_board-manage_links-[a-z]{15}_id",
      names(app$get_values(input = TRUE)[["input"]]),
      value = TRUE
    )

    setdiff(sub("id$", "", res), curr)
  }

  set_input <- function(val, name, app) {
    do.call(app$set_inputs, set_names(list(val), name))
    invisible()
  }

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

  app$click("my_board-manage_links-add_link")
  app$wait_for_idle()
  inp <- links_input_ids()

  Map(
    set_input,
    list("a", "c", "x"),
    paste0(inp, c("from", "to", "input")),
    MoreArgs = list(app = app)
  )

  app$click("my_board-manage_links-add_link")
  app$wait_for_idle()
  inp <- links_input_ids(inp)

  Map(
    set_input,
    list("b", "c", "y"),
    paste0(inp, c("from", "to", "input")),
    MoreArgs = list(app = app)
  )

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

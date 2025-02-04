test_that("add/rm blocks", {

  testServer(
    add_rm_block_server,
    {
      expect_null(res$add)
      expect_null(res$rm)

      session$setInputs(
        registry_select = "dataset_block",
        block_id = "a",
        confirm_add = 1
      )

      expect_s3_class(res$add, "blocks")
      expect_length(res$add, 1L)
      expect_null(res$rm)
    },
    args = list(rv = reactiveValues(board = new_board()))
  )

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  testServer(
    add_rm_block_server,
    {
      expect_null(res$add)
      expect_null(res$rm)

      session$setInputs(block_select = "a", confirm_rm = 1)

      expect_type(res$rm, "character")
      expect_length(res$rm, 1L)
      expect_null(res$add)
    },
    args = list(rv = reactiveValues(board = board))
  )
})

test_that("dummy add/rm block ui test", {
  expect_s3_class(add_rm_block_ui("add_rm", new_board()), "shiny.tag.list")
})

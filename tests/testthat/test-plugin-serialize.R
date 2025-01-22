test_that("add/rm blocks", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(from = "a", to = "b")
  )

  temp <- tempfile(fileext = ".json")

  testServer(
    ser_deser_server,
    {
      file.copy(output$serialize, temp)
    },
    args = list(rv = reactiveValues(board = board))
  )

  testServer(
    ser_deser_server,
    {
      session$setInputs(restore = list(datapath = temp))

      expect_s3_class(res(), class(board))

      expect_length(board_blocks(res()), length(board_blocks(board)))
      expect_length(board_links(res()), length(board_links(board)))

      expect_setequal(board_block_ids(res()), board_block_ids(board))
      expect_setequal(board_link_ids(res()), board_link_ids(board))
    },
    args = list(rv = reactiveValues(board = new_board()))
  )
})

test_that("dummy ser/deser ui test", {
  expect_s3_class(ser_deser_ui("ser_deser", new_board()), "shiny.tag.list")
})

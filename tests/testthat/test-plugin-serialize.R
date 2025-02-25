test_that("ser/deser module", {

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

test_that("ser/deser board", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("BOD"),
      c = new_merge_block(by = "Time")
    ),
    links = links(
      from = c("a", "b"),
      to = c("c", "c"),
      input = c("x", "y")
    )
  )

  temp <- tempfile(fileext = ".json")

  testServer(
    get_s3_method("board_server", board),
    {
      ser_deser <- session$makeScope("preserve_board")
      file.copy(ser_deser$output$serialize, temp)
    },
    args = list(x = board, plugins = list(preserve_board = ser_deser_server))
  )

  testServer(
    get_s3_method("board_server", board),
    {
      ser_deser <- session$makeScope("preserve_board")
      ser_deser$setInputs(restore = list(datapath = temp))

      brd <- rv$board

      expect_length(board_blocks(brd), length(board_blocks(board)))
      expect_length(board_links(brd), length(board_links(board)))

      expect_setequal(board_block_ids(brd), board_block_ids(board))
      expect_setequal(board_link_ids(brd), board_link_ids(board))
    },
    args = list(
      x = new_board(),
      plugins = list(preserve_board = ser_deser_server)
    )
  )
})

test_that("gen_code return validation", {

  with_mock_session(
    {
      check_ser_deser_val(list(a = 1))
      sink_msg(
        expect_warning(
          session$flushReact(),
          "Expecting `preserve_board` to return a reactive value"
        )
      )
    }
  )

  with_mock_session(
    {
      check_ser_deser_val(reactiveVal(1))
      sink_msg(
        expect_warning(
          session$flushReact(),
          paste(
            "Expecting the `preserve_board` return value to evaluate to a",
            "`board` object."
          )
        )
      )
    }
  )
})

test_that("dummy ser/deser ui test", {
  expect_s3_class(ser_deser_ui("ser_deser", new_board()), "shiny.tag.list")
})

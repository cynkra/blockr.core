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

  testServer(
    add_rm_block_server,
    {
      expect_null(res$add)
      expect_null(res$rm)

      session$setInputs(confirm_add = 1)
      session$setInputs(block_id = "", confirm_add = 2)
      session$setInputs(block_id = "a", confirm_add = 3)
      session$setInputs(block_id = "c", registry_select = "abc",
                        confirm_add = 4)
      session$setInputs(cancel_add = 1)

      session$setInputs(confirm_rm = 1)
      session$setInputs(block_select = "x", confirm_rm = 2)
      session$setInputs(cancel_rm = 1)

      expect_null(res$add)
      expect_null(res$rm)
    },
    args = list(rv = reactiveValues(board = board))
  )
})

test_that("add/rm blocks return validation", {

  with_mock_session(
    {
      check_add_rm_block_val(list(), list())
      sink_msg(
        expect_warning(
          session$flushReact(),
          "Expecting `manage_blocks` to return a `reactivevalues` object"
        )
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_block_val(reactiveValues(abc = 1), list())
      sink_msg(
        expect_warning(
          session$flushReact(),
          paste(
            "Expecting the `manage_blocks` return value to contain",
            "components `add` and `rm`"
          )
        )
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_block_val(reactiveValues(add = "a"), list())
      sink_msg(
        expect_warning(
          session$flushReact(),
          paste(
            "Expecting the `add` component of the `manage_blocks` return",
            "value to be `NULL` or a `blocks` object"
          )
        )
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_block_val(
        list(add = blocks(a = new_dataset_block())),
        list(board = new_board(blocks = blocks(a = new_dataset_block())))
      )
      sink_msg(
        expect_warning(
          session$flushReact(),
          "Expecting the newly added block to have a unique ID"
        )
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_block_val(
        reactiveValues(add = NULL, rm = 1),
        list(board = new_board())
      )
      sink_msg(
        expect_warning(
          session$flushReact(),
          paste(
            "Expecting the `rm` component of the `manage_blocks` return",
            "value to be `NULL` or a character vector"
          )
        )
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_block_val(
        reactiveValues(add = NULL, rm = "a"),
        list(board = new_board())
      )
      sink_msg(
        expect_warning(
          session$flushReact(),
          "Expecting the removed block to be specified by a known ID"
        )
      )
    }
  )
})

test_that("dummy add/rm block ui test", {
  expect_s3_class(add_rm_block_ui("add_rm", new_board()), "shiny.tag.list")
})

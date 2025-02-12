test_that("add/rm stacks", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    )
  )

  testServer(
    add_rm_stack_server,
    {
      expect_identical(upd$add, stacks())
      expect_identical(upd$rm, character())
      expect_null(upd$edit)
      expect_identical(res(), list(add = NULL, rm = NULL))

      session$setInputs(add_stack = 1)

      session$flushReact()

      expect_s3_class(upd$add, "stacks")
      expect_length(upd$add, 1L)

      stk <- names(upd$add)
      new <- as_stacks(
        set_names(list(new_stack(name = stack_name(upd$add[[1L]]))), stk)
      )

      expect_identical(upd$add, new)

      upd$edit <- list(row = names(upd$add), col = "blocks", val = "a")
      stack_blocks(new[[1]]) <- "a"

      session$flushReact()
      expect_identical(upd$add, new)

      upd$edit <- list(row = names(upd$add), col = "name", val = "my stack")
      stack_name(new[[1]]) <- "my stack"

      session$flushReact()
      expect_identical(upd$add, new)

      session$setInputs(modify_stacks = 1)

      expect_identical(
        res(),
        list(add = new, rm = character())
      )

      expect_identical(upd$add, stacks())
      expect_identical(upd$rm, character())
    },
    args = list(rv = reactiveValues(board = board))
  )

  testServer(
    add_rm_stack_server,
    {
      expect_null(upd$edit)
      expect_length(upd$add, 0L)

      session$flushReact()

      expect_length(upd$obs, 1L)
      expect_named(upd$obs, "ac")
      expect_type(upd$obs, "list")

      expect_length(upd$obs[["ac"]], 2L)
      expect_named(upd$obs[["ac"]], c("name", "blocks"))
      expect_type(upd$obs[["ac"]], "list")

      for (i in c("name", "blocks")) {
        expect_s3_class(upd$obs[["ac"]][[i]], "Observer")
      }

      session$setInputs(ac_name = "some stack")
      expect_identical(
        upd$edit,
        list(row = "ac", col = "name", val = "some stack")
      )
      expect_length(upd$add, 1L)

      session$setInputs(ac_blocks = c("a", "b"))
      expect_identical(
        upd$edit,
        list(row = "ac", col = "blocks", val = c("a", "b"))
      )
      expect_length(upd$add, 1L)
    },
    args = list(
      rv = list(
        board = new_board(
          blocks = c(
            a = new_dataset_block("iris"),
            b = new_dataset_block("mtcars"),
            c = new_subset_block(),
            d = new_subset_block()
          ),
          stacks = stacks(ac = c("a", "c"))
        )
      )
    )
  )

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    stacks = stacks(ab = c("a", "b"))
  )

  testServer(
    add_rm_stack_server,
    {
      expect_s3_class(upd$curr, "stacks")
      expect_length(upd$curr, 1L)
      expect_named(upd$curr, "ab")

      expect_identical(upd$rm, character())

      session$setInputs(stacks_dt_rows_selected = 1, rm_stack = 1)

      expect_identical(upd$rm, "ab")

      session$setInputs(modify_stacks = 1)

      expect_identical(
        res(),
        list(add = stacks(), rm = "ab")
      )

      expect_identical(upd$add, stacks())
      expect_identical(upd$rm, character())
      expect_identical(upd$curr, stacks())
    },
    args = list(rv = reactiveValues(board = board))
  )
})

test_that("add/rm stacks return validation", {

  with_mock_session(
    {
      val <- reactiveVal(
        list(add = stacks(a = "a"), rm = "ab")
      )

      rv <- list(
        board = new_board(
          blocks = c(
            a = new_dataset_block("iris"),
            b = new_subset_block()
          ),
          stacks = stacks(ab = c("a", "b"))
        )
      )

      res <- check_add_rm_stack_val(val, rv)

      expect_s3_class(isolate(res()$add), "stacks")
      expect_length(isolate(res()$add), 1L)

      expect_type(isolate(res()$rm), "character")
      expect_length(isolate(res()$rm), 1L)


      val(list(add = NULL, rm = "bc"))

      sink_msg(
        expect_warning(
          session$flushReact(),
          "Expecting all stack IDs to be removed to be known"
        )
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_stack_val(list(), list())
      sink_msg(
        expect_warning(
          session$flushReact(),
          "Expecting `manage_stacks` to return a reactive value"
        )
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_stack_val(reactiveVal(1), list())
      sink_msg(
        expect_warning(
          session$flushReact(),
          paste(
            "Expecting the `manage_stacks` return value to evaluate to a list",
            "with components `add` and `rm`"
          )
        )
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_stack_val(reactiveVal(list(add = 1, rm = NULL)), list())
      sink_msg(
        expect_warning(
          session$flushReact(),
          paste(
            "Expecting the `add` component of the `manage_stacks` return",
            "value to be `NULL` or a `stacks` object"
          )
        )
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_stack_val(
        reactiveVal(
          list(
            add = structure(list(1), class = class(stacks())),
            rm = NULL
          )
        ),
        list()
      )

      sink_msg(
        expect_warning(session$flushReact(), "Error in validate_stack")
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_stack_val(reactiveVal(list(add = NULL, rm = 1)), list())
      sink_msg(
        expect_warning(
          session$flushReact(),
          paste(
            "Expecting the `rm` component of the `manage_stacks` return",
            "value to be a character vector"
          )
        )
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_stack_val(
        reactiveVal(list(add = NULL, rm = "a")),
        list(board = new_board())
      )
      sink_msg(
        expect_warning(
          session$flushReact(),
          "Expecting all stack IDs to be removed to be known"
        )
      )
    }
  )
})

test_that("dummy ad/rm stack ui test", {
  expect_s3_class(add_rm_stack_ui("stack", new_board()), "shiny.tag.list")
  expect_s3_class(stacks_modal(NS("stacks")), "shiny.tag")
})

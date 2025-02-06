test_that("add/rm links", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    )
  )

  testServer(
    add_rm_link_server,
    {
      expect_identical(upd$add, links())
      expect_identical(upd$rm, character())
      expect_null(upd$edit)
      expect_identical(res(), list(add = NULL, rm = NULL))

      session$setInputs(add_link = 1)

      expect_s3_class(upd$add, "links")
      expect_length(upd$add, 1L)

      lnk <- names(upd$add)
      expect_identical(upd$add, as_links(set_names(list(new_link()), lnk)))

      session$setInputs(add_link = 2)

      expect_identical(upd$add, as_links(set_names(list(new_link()), lnk)))

      upd$edit <- list(row = names(upd$add), col = "from", val = "a")
      session$flushReact()
      expect_identical(upd$add, as_links(set_names(list(new_link("a")), lnk)))

      upd$edit <- list(row = names(upd$add), col = "to", val = "b")
      session$flushReact()
      expect_identical(
        upd$add,
        as_links(set_names(list(new_link("a", "b")), lnk))
      )

      upd$edit <- list(row = names(upd$add), col = "input", val = "data")
      session$flushReact()
      expect_identical(
        upd$add,
        as_links(set_names(list(new_link("a", "b", "data")), lnk))
      )

      session$setInputs(modify_links = 1)

      expect_identical(
        res(),
        list(
          add = as_links(set_names(list(new_link("a", "b", "data")), lnk)),
          rm = character()
        )
      )

      expect_identical(upd$add, links())
      expect_identical(upd$rm, character())
    },
    args = list(rv = reactiveValues(board = board))
  )

  testServer(
    add_rm_link_server,
    {
      expect_null(upd$edit)
      expect_length(upd$add, 0L)

      session$flushReact()

      expect_length(upd$obs, 1L)
      expect_named(upd$obs, "ac")
      expect_type(upd$obs, "list")

      expect_length(upd$obs[["ac"]], 4L)
      expect_named(upd$obs[["ac"]], c("id", "from", "to", "input"))
      expect_type(upd$obs[["ac"]], "list")

      for (i in c("id", "from", "to", "input")) {
        expect_s3_class(upd$obs[["ac"]][[i]], "Observer")
      }

      session$setInputs(ac_input = "data")
      expect_null(upd$edit)
      expect_length(upd$add, 0L)

      session$setInputs(ac_to = "d")
      expect_identical(upd$edit, list(row = "ac", col = "to", val = "d"))
      expect_length(upd$add, 1L)

      session$setInputs(ac_from = "b")
      expect_identical(upd$edit, list(row = "ac", col = "from", val = "b"))
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
          links = links(ac = new_link(from = "a", to = "c"))
        )
      )
    )
  )

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    links = links(aa = new_link(from = "a", to = "b"))
  )

  testServer(
    add_rm_link_server,
    {
      expect_s3_class(upd$curr, "links")
      expect_length(upd$curr, 1L)
      expect_named(upd$curr, "aa")

      expect_identical(upd$rm, character())

      session$setInputs(links_rows_selected = 1, rm_link = 1)

      expect_identical(upd$rm, "aa")

      session$setInputs(modify_links = 1)

      expect_identical(
        res(),
        list(add = links(), rm = "aa")
      )

      expect_identical(upd$add, links())
      expect_identical(upd$rm, character())
      expect_identical(upd$curr, links())
    },
    args = list(rv = reactiveValues(board = board))
  )
})

test_that("add/rm links return validation", {

  with_mock_session(
    {
      val <- reactiveVal(
        list(add = links(from = "a", to = "b"), rm = "ab")
      )

      rv <- list(
        board = new_board(
          blocks = c(
            a = new_dataset_block("iris"),
            b = new_subset_block()
          ),
          links = links(ab = new_link(from = "a", to = "b"))
        )
      )

      res <- check_add_rm_link_val(val, rv)

      expect_s3_class(isolate(res()$add), "links")
      expect_length(isolate(res()$add), 1L)

      expect_type(isolate(res()$rm), "character")
      expect_length(isolate(res()$rm), 1L)


      val(list(add = NULL, rm = "bc"))

      sink_msg(
        expect_warning(session$flushReact(), "Error in observe")
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_link_val(list(), list())
      sink_msg(
        expect_warning(session$flushReact(), "Error in observe")
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_link_val(reactiveVal(1), list())
      sink_msg(
        expect_warning(session$flushReact(), "Error in observe")
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_link_val(reactiveVal(list(add = 1, rm = NULL)), list())
      sink_msg(
        expect_warning(session$flushReact(), "Error in observe")
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_link_val(
        reactiveVal(
          list(
            add = structure(
              list(id = "x", from = "a", to = "a", input = ""),
              class = class(links())
            ),
            rm = NULL
          )
        ),
        list()
      )

      sink_msg(
        expect_warning(session$flushReact(), "Error in validate_link")
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_link_val(reactiveVal(list(add = NULL, rm = 1)), list())
      sink_msg(
        expect_warning(session$flushReact(), "Error in observe")
      )
    }
  )

  with_mock_session(
    {
      check_add_rm_link_val(reactiveVal(list(add = NULL, rm = 1)), list())
      sink_msg(
        expect_warning(session$flushReact(), "Error in observe")
      )
    }
  )
})

test_that("dummy ad/rm link ui test", {
  expect_s3_class(add_rm_link_ui("link", new_board()), "shiny.tag.list")
  expect_s3_class(links_modal(NS("links")), "shiny.tag")
})

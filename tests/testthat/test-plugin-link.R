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

test_that("dummy ad/rm link ui test", {
  expect_s3_class(add_rm_link_ui("link", new_board()), "shiny.tag.list")
})

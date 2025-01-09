test_that("topo sort", {

  # linear graph
  expect_identical(
    topo_sort(letters[1:4], data.frame(from = letters[1:3],
                                         to = letters[2:4])),
    letters[1:4]
  )

  # diverging paths
  expect_identical(
    topo_sort(letters[1:4], data.frame(from = c("a", "a", "b", "c"),
                                         to = c("b", "c", "d", "d"))),
    c("a", "c", "b", "d") # or c("a", "b", "c", "d")
  )

  # multiple roots
  expect_identical(
    topo_sort(letters[1:4], data.frame(from = c("a", "c"),
                                         to = c("b", "d"))),
    c("c", "d", "a", "b") # or c("a", "b", "c", "d")
  )

  # cycle
  expect_error(
    topo_sort(
      letters[1:4],
      data.frame(from = c("a", "b", "c"),
                   to = c("b", "c", "a"))
    ),
    "The graph contains a cycle and is not a DAG."
  )

  # single node
  expect_identical(
    topo_sort("a", data.frame(from = character(), to = character())),
    "a"
  )

  # empty graph
  expect_identical(
    topo_sort(character(), data.frame(from = character(), to = character())),
    character()
  )
})

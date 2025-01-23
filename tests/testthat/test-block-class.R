test_that("block constructor", {

  new_identity_block <- function() {
    new_transform_block(
      function(data) {
        moduleServer(
          "expression",
          function(input, output, session) {
            list(
              expr = reactive(quote(identity(data))),
              state = list()
            )
          }
        )
      },
      function(id) {
        tagList()
      },
      class = "identity_block"
    )
  }

  expect_s3_class(
    new_identity_block(),
    c("identity_block", "transform_block", "block")
  )
})

test_that("blocks utils", {

  a <- new_dataset_block()
  b <- new_subset_block()

  ab <- c(a, b)

  expect_s3_class(ab, "blocks")
  expect_length(ab, 2L)
  expect_named(ab)

  names(ab) <- c("a", "b")

  expect_named(ab, c("a", "b"))

  names(ab)[2] <- "c"

  expect_named(ab, c("a", "c"))
})

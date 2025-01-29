test_that("block constructor", {

  new_identity_block <- function() {
    new_transform_block(
      function(id, data) {
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

test_that("block class", {

  x <- new_dataset_block()

  expect_s3_class(x, "block")
  expect_true(is_block(x))
  expect_false(is_block("x"))

  expect_identical(x, as_block(x))
  expect_equal(x, as_block(as.list(x)), ignore_function_env = TRUE)

  expect_snapshot(print(x))
})

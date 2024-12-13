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
      function(ns) {
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

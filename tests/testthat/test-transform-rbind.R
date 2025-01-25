test_that("rbind block constructor", {

  blk <- new_rbind_block()

  expect_s3_class(blk, "rbind_block")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_identical(
        session$returned$result(),
        rbind(iris[1:3, ], iris[4:6, ])
      )
    },
    args = list(
      x = blk,
      data = list(
        function() iris[1:3, ],
        function() iris[4:6, ]
      )
    )
  )

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_identical(
        session$returned$result(),
        rbind(a = iris[1:3, ], b = iris[4:6, ])
      )
    },
    args = list(
      x = blk,
      data = list(
        a = function() iris[1:3, ],
        b = function() iris[4:6, ]
      )
    )
  )
})

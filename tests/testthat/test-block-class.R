test_that("block constructor", {
  blk <- new_block(quote(identity()), "identity_block")
  expect_s3_class(blk, c("identity_block", "block"))
})

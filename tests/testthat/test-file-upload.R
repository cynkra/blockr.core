test_that("upload block constructor", {

  file <- withr::local_tempfile(lines = "abc")

  blk <- new_upload_block()

  expect_s3_class(blk, "upload_block")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$makeScope("expr")$setInputs(upload = list(datapath = file))
      session$flushReact()
      expect_identical(
        session$returned$result(),
        file
      )
    },
    args = list(x = blk)
  )
})

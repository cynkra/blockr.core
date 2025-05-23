skip_on_os("windows")

test_that("filebrowser block constructor", {

  file <- normalizePath(withr::local_tempfile(lines = "abc"))

  blk <- new_filebrowser_block(volumes = c(tmp = dirname(file)))

  expect_s3_class(blk, "filebrowser_block")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$makeScope("expr")$setInputs(
        file = list(files = basename(file), root = "tmp")
      )
      session$flushReact()
      expect_identical(
        session$returned$result(),
        file
      )
    },
    args = list(x = blk)
  )
})

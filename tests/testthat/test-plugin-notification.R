test_that("add/rm blocks", {

  testServer(
    block_notification_server,
    {
      expect_null(session$returned())
      expect_identical(get_globals("notification_ids"), list())

      rv$blocks <- list(
        a = list(
          server = list(
            cond = function() {
              list(
                data = list(),
                state = list(),
                eval = list(
                  error = list(structure("some error", id = "abc")),
                  warning = character(),
                  message = character()
                )
              )
            }
          )
        )
      )

      expect_identical(
        session$returned(),
        list(
          a = list(
            eval = list(error = list(structure("some error", id = "abc")))
          )
        )
      )

      session$flushReact()

      expect_identical(
        get_globals("notification_ids"),
        list(a = "a_abc")
      )

      rv$blocks <- list()

      expect_null(session$returned())

      session$flushReact()

      expect_identical(
        get_globals("notification_ids"),
        list(a = NULL)[0]
      )
    },
    args = list(rv = reactiveValues(blocks = list()))
  )
})

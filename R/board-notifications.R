#' Block notification module
#'
#' Object (de)serialization in a board server context.
#'
#' @param rv Reactive values object
#'
#' @return A [shiny::reactiveVal()] object that evaluates to `NULL` or a
#' `board` obejct.
#'
#' @rdname block_notifications
#' @export
block_notification_server <- function(rv) {
  moduleServer(
    "block_notifications",
    function(input, output, session) {

      cnd <- reactive(
        lst_xtr_reval(rv$blocks, "server", "cond")
      )

      ids <- list()

      observeEvent(
        cnd(),
        {
          notf <- cnd()

          for (blk in setdiff(names(ids), names(notf))) {

            for (id in ids[[blk]]) {
              removeNotification(id)
            }

            ids[[blk]] <- NULL
          }

          for (blk in names(notf)) {

            cur <- c()

            for (typ in names(notf[[blk]])) {
              for (cnd in names(notf[[blk]][[typ]])) {
                for (msg in notf[[blk]][[typ]][[cnd]]) {

                  id <- paste0(blk, "_", attr(msg, "id"))

                  showNotification(
                    paste0("Block ", blk, ": ", msg),
                    duration = NULL,
                    id = id,
                    type = cnd
                  )

                  cur <- c(cur, id)
                }
              }
            }

            for (id in setdiff(ids[[blk]], cur)) {
              removeNotification(id)
            }

            ids[[blk]] <<- cur
          }
        }
      )

      reactive(
        filter_all_zero_len(cnd())
      )
    }
  )
}

check_block_notifications_val <- function(val) {

  observeEvent(
    TRUE,
    {
      if (!is.reactive(val)) {
        stop("Expecting a `block_notifications` server to return a reactive ",
             "value.")
      }
    },
    once = TRUE
  )

  observeEvent(
    val(),
    {
      if (!is.list(val())) {
        stop("Expecting the `block_notifications` return value to evaluate ",
             "to a list.")
      }
    },
    once = TRUE
  )

  val
}

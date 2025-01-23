#' Block notification module
#'
#' Object (de)serialization in a board server context.
#'
#' @param id Namespace ID
#' @param rv Reactive values object
#'
#' @return A [shiny::reactiveVal()] object that evaluates to `NULL` or a
#' `board` obejct.
#'
#' @rdname block_notifications
#' @export
block_notification_server <- function(id, rv) {
  moduleServer(
    id,
    function(input, output, session) {

      onStop(function() set_globals("notification_ids", list()))

      if (length(get_globals("notification_ids"))) {
        warning("Existing notification IDs will be purged.")
      }

      set_globals("notification_ids", list())

      cnd <- reactive(
        lst_xtr_reval(rv$blocks, "server", "cond")
      )

      observeEvent(
        cnd(),
        {
          notf <- cnd()
          ids <- get_globals("notification_ids")

          for (blk in setdiff(names(ids), names(notf))) {

            for (id in ids[[blk]]) {
              removeNotification(id)
            }

            ids[[blk]] <- NULL
            set_globals("notification_ids", ids)
          }

          for (blk in names(notf)) {

            cur <- create_block_notifications(notf, blk)

            for (id in setdiff(ids[[blk]], cur)) {
              removeNotification(id)
            }

            ids[[blk]] <- cur
            set_globals("notification_ids", ids)
          }
        }
      )

      reactive(
        filter_all_zero_len(cnd())
      )
    }
  )
}

create_block_notifications <- function(notf, blk) {

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

  cur
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

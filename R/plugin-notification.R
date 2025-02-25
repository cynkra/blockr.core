#' Block notification module
#'
#' Object (de)serialization in a board server context.
#'
#' @param id Namespace ID
#' @param rv Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @return A [shiny::reactiveVal()] object that evaluates to `NULL` or a
#' `board` obejct.
#'
#' @rdname block_notifications
#' @export
block_notification_server <- function(id, rv, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      onStop(
        function() set_globals(list(), session = session)
      )

      if (length(get_globals(session = session))) {
        warning("Existing notification IDs will be purged.")
      }

      set_globals(list(), session = session)

      cnd <- reactive(
        lst_xtr_reval(rv$blocks, "server", "cond")
      )

      observeEvent(
        cnd(),
        {
          notf <- cnd()
          ids <- get_globals(session = session)

          for (blk in setdiff(names(ids), names(notf))) {

            for (id in ids[[blk]]) {
              removeNotification(id)
            }

            ids[[blk]] <- NULL
            set_globals(ids, session = session)
          }

          for (blk in names(notf)) {

            cur <- create_block_notifications(notf, blk)

            for (id in setdiff(ids[[blk]], cur)) {
              removeNotification(id)
            }

            ids[[blk]] <- cur
            set_globals(ids, session = session)
          }
        }
      )

      reactive(
        filter_all_zero_len(cnd())
      )
    }
  )
}

create_block_notifications <- function(notf, blk,
                                       session = getDefaultReactiveDomain()) {

  cur <- c()

  for (typ in names(notf[[blk]])) {
    for (cnd in names(notf[[blk]][[typ]])) {
      for (msg in notf[[blk]][[typ]][[cnd]]) {

        id <- session_to_id(attr(msg, "id"), session)

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
        abort(
          "Expecting `notify_user` to return a reactive value.",
          class = "notify_user_return_invalid"
        )
      }
    },
    once = TRUE
  )

  observeEvent(
    val(),
    {
      if (!is.list(val())) {
        abort(
          "Expecting the `notify_user` return value to evaluate to a list.",
          class = "notify_user_return_invalid"
        )
      }
    },
    once = TRUE
  )

  val
}

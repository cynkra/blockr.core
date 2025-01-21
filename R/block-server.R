#' Generics for server generation
#'
#' Calls shiny modules for the given element (block, fields).
#'
#' @param x Object for which to generate a [moduleServer()]
#' @param data Input data (list of reactives)
#' @param id Block ID
#' @param ... Generic consistency
#'
#' @export
block_server <- function(x, data, id = "block", ...) {
  UseMethod("block_server")
}

#' @rdname block_server
#' @export
block_server.block <- function(x, data, id = "block", ...) {
  moduleServer(
    id,
    function(input, output, session) {

      cond_msg <- local(
        {
          id <- 1L

          function(cnd) {

            id <<- id + 1L

            if (inherits(cnd, "condition")) {
              cnd <- conditionMessage(cnd)
            }

            list(
              structure(cnd, id = int_to_chr(id), class = "block_cnd")
            )
          }
        }
      )

      empty_cond <- list(
        error = character(),
        warning = character(),
        message = character()
      )

      rv <- reactiveValues(
        data_valid = if (block_has_data_validator(x)) NULL else TRUE,
        data_cond = empty_cond,
        state_set = NULL,
        state_cond = empty_cond,
        eval_cond = empty_cond
      )

      res <- reactiveVal()

      exp <- check_expr_val(
        expr_server(x, data),
        x
      )

      if (block_has_data_validator(x)) {

        observeEvent(
          lapply(data, reval),
          {
            res(NULL)

            rv$data_cond <- empty_cond
            rv$state_set <- NULL

            rv$data_valid <- tryCatch(
              withCallingHandlers(
                {
                  validate_data_inputs(x, lapply(data, reval))
                  TRUE
                },
                message = function(m) {
                  rv$data_cond$message <- c(rv$data_cond$message, cond_msg(m))
                },
                warning = function(w) {
                  rv$data_cond$warning <- c(rv$data_cond$warning, cond_msg(w))
                }
              ),
              error = function(e) {
                rv$data_cond$error <- cond_msg(e)
                NULL
              }
            )
          }
        )
      }

      state_check <- reactive(
        {
          if (!isTruthy(rv$data_valid)) {
            return(NULL)
          }

          allow_empty <- block_allow_empty_state(x)

          if (isTRUE(allow_empty)) {
            return(TRUE)
          }

          if (isFALSE(allow_empty)) {
            check <- TRUE
          } else {
            check <- allow_empty
          }

          lgl_ply(
            lapply(exp$state[check], reval_if),
            isTruthy,
            use_names = TRUE
          )
        }
      )

      observeEvent(
        state_check(),
        {
          res(NULL)

          ok <- state_check()

          rv$state_cond <- empty_cond
          rv$state_set <- NULL

          if (!all(ok)) {
            rv$state_cond$error <- cond_msg(
              paste0("State values ", paste_enum(names(ok)[!ok]), " are ",
                     "not yet initialized.")
            )
          } else {
            rv$state_set <- TRUE
          }
        }
      )

      dat_eval <- reactive(
        {
          req(rv$state_set)
          lapply(exp$state, reval_if)
          lapply(data, reval)
        }
      )

      observeEvent(
        dat_eval(),
        {
          rv$eval_cond <- empty_cond

          out <- tryCatch(
            withCallingHandlers(
              eval(exp$expr(), dat_eval()),
              message = function(m) {
                rv$eval_cond$message <- c(rv$eval_cond$message, cond_msg(m))
              },
              warning = function(w) {
                rv$eval_cond$warning <- c(rv$eval_cond$warning, cond_msg(w))
              }
            ),
            error = function(e) {
              rv$eval_cond$error <- cond_msg(e)
              NULL
            }
          )

          res(out)
        }
      )

      output$result <- block_output(x, res)

      list(
        result = res,
        expr = exp$expr,
        state = exp$state,
        cond = reactive(
          list(
            data = rv$data_cond,
            state = rv$state_cond,
            eval = rv$eval_cond
          )
        )
      )
    }
  )
}

#' @rdname block_server
#' @export
expr_server <- function(x, data, ...) {
  UseMethod("expr_server")
}

#' @rdname block_server
#' @export
expr_server.block <- function(x, data, ...) {
  do.call(block_expr_server(x), c(list(id = "expr"), data))
}

check_expr_val <- function(val, x) {

  observeEvent(
    val,
    {
      if (!setequal(names(val), c("expr", "state"))) {
        stop("The block server for ", class(x)[1L],
             " is expected to return values `expr` and `state`.")
      }

      if (!is.reactive(val[["expr"]])) {
        stop("The `expr` component of the return value for ", class(x)[1L],
             " is expected to be a reactive.")
      }

      expected <- block_ctor_inputs(x)
      current <- names(val[["state"]])

      if (!setequal(current, expected)) {
        stop("The `state` component of the return value for ", class(x)[1L],
             " is expected to return ",
             paste0("`", setdiff(expected, current), "`", collapse = ", "))
      }
    },
    once = TRUE
  )

  val
}

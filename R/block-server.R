#' Generics for server generation
#'
#' Calls shiny modules for the given element (block, fields).
#'
#' @param id Block ID
#' @param x Object for which to generate a [moduleServer()]
#' @param data Input data (list of reactives)
#' @param ... Generic consistency
#'
#' @export
block_server <- function(id, x, data = list(), ...) {
  UseMethod("block_server", x)
}

#' @rdname block_server
#' @export
block_server.block <- function(id, x, data = list(), ...) {
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

      if ("...args" %in% names(data)) {

        observeEvent(
          names(data[["...args"]]),
          {
            arg_names <- names(data[["...args"]])
            pos_args <- grepl("[1-9][0-9]*", arg_names)

            if (any(pos_args)) {

              ind <- which(pos_args)
              ind <- c(
                ind[order(as.integer(arg_names[ind]))],
                which(!pos_args)
              )

              reorder_rv(data[["...args"]], arg_names[ind])
            }
          }
        )
      }

      res <- reactiveVal()

      exp <- check_expr_val(
        expr_server(x, data),
        x
      )

      dat <- reactive(
        {
          res <- lapply(data[names(data) != "...args"], reval)

          if ("...args" %in% names(data)) {
            tmp <- list(`...args` = reactiveValuesToList(data[["...args"]]))
            res <- c(res, tmp)
          }

          res
        }
      )

      if (block_has_data_validator(x)) {

        observeEvent(
          dat(),
          {
            log_debug("performing input validation for block ", id)

            res(NULL)

            rv$data_cond <- empty_cond
            rv$state_set <- NULL

            rv$data_valid <- tryCatch(
              withCallingHandlers(
                {
                  validate_data_inputs(x, dat())
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
          dat()

          if (!isTruthy(rv$data_valid)) {
            return(NULL)
          }

          allow_empty <- block_allow_empty_state(x)

          if (isTRUE(allow_empty) || !length(exp$state)) {
            return(TRUE)
          }

          if (isFALSE(allow_empty)) {
            check <- TRUE
          } else {
            check <- setdiff(names(exp$state), allow_empty)
          }

          lgl_ply(
            lapply(exp$state[check], reval_if),
            Negate(is_empty),
            use_names = TRUE
          )
        }
      )

      observeEvent(
        state_check(),
        {
          log_debug("checking returned state values of block ", id)

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
          try(exp$expr(), silent = TRUE)

          res <- dat()

          if ("...args" %in% names(res)) {
            res <- c(res[names(res) != "...args"], res[["...args"]])
          }

          res
        }
      )

      observeEvent(
        dat_eval(),
        {
          log_debug("evaluating block ", id)

          rv$eval_cond <- empty_cond

          out <- tryCatch(
            withCallingHandlers(
              {
                block_eval(x, exp$expr(), dat_eval())
              },
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

      observeEvent(
        res(),
        {
          output$result <- block_output(x, res())
        }
      )

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

#' @rdname block_server
#' @export
block_eval <- function(x, expr, data, ...) {
  UseMethod("block_eval")
}

#' @param expr Quoted expression to evaluate in the context of `data`
#' @rdname block_server
#' @export
block_eval.block <- function(x, expr, data, ...) {
  eval(expr, data)
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

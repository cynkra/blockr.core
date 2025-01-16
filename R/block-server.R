#' Generics for server generation
#'
#' Calls shiny modules for the given element (block, fields).
#'
#' @param x Object for which to generate a [moduleServer()]
#' @param data Input data (list of reactives)
#' @param ... Generic consistency
#'
#' @export
block_server <- function(x, data, ...) {
  UseMethod("block_server")
}

#' @rdname block_server
#' @export
block_server.block <- function(x, data, ...) {
  moduleServer(
    block_uid(x),
    function(input, output, session) {

      empty_cond <- list(
        error = character(),
        warnings = character(),
        messages = character()
      )

      rv <- reactiveValues(
        data_valid = if (block_has_dat_val(x)) NULL else TRUE,
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

      if (block_has_dat_val(x)) {

        observeEvent(
          lapply(data, reval),
          {
            rv$data_cond <- empty_cond
            rv$state_set <- NULL

            rv$data_valid <- tryCatch(
              withCallingHandlers(
                {
                  validate_data_inputs(x, lapply(data, reval))
                  TRUE
                },
                messages = function(m) {
                  rv$data_cond$messages <- c(
                    rv$data_cond$messages,
                    conditionMessage(m)
                  )
                },
                warnings = function(w) {
                  rv$data_cond$warnings <- c(
                    rv$data_cond$warnings,
                    conditionMessage(w)
                  )
                }
              ),
              error = function(e) {
                rv$data_cond$error <- conditionMessage(e)
                NULL
              }
            )
          }
        )
      }

      observe(
        {
          req(rv$data_valid)

          state <- lgl_ply(
            lapply(exp$state, reval_if),
            isTruthy,
            use_names = TRUE
          )

          if (!all(state)) {
            rv$state_cond$error <- paste0(
              "State values ", paste_enum(names(state)[!state]), " are not ",
              "yet initialized."
            )
          } else {
            rv$state_set <- TRUE
          }
        }
      )

      observe(
        {
          req(rv$state_set)

          rv$eval_cond <- empty_cond

          out <- tryCatch(
            withCallingHandlers(
              eval(exp$expr(), lapply(data, reval)),
              messages = function(m) {
                rv$eval_cond$messages <- c(
                  rv$eval_cond$messages,
                  conditionMessage(m)
                )
              },
              warnings = function(w) {
                rv$eval_cond$warnings <- c(
                  rv$eval_cond$warnings,
                  conditionMessage(w)
                )
              }
            ),
            error = function(e) {
              rv$eval_cond$error <- conditionMessage(e)
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
        json = reactive(to_json(x, lapply(exp$state, reval_if))),
        cond = reactive(
          list(
            data = data_cond,
            state = state_cond,
            eval = eval_cond
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
  res <- do.call(block_expr_server(x), data)
  stopifnot(setequal(names(res), c("expr", "state")))
  res
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

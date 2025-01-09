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

      exp <- expr_server(x, data)

      check_expr_server_return_value(x, exp)

      res <- reactive({
        tryCatch(
          eval(exp$expr(), lapply(data, reval)),
          error = function(e) {
            showNotification(conditionMessage(e), duration = NULL,
                             type = "error")
            NULL
          }
        )
      })

      output$result <- block_output(x, res)

      list(
        result = res,
        expr = exp$expr,
        json = reactive(to_json(x, lapply(exp$state, reval_if)))
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

check_expr_server_return_value <- function(x, ret) {

  observeEvent(
    ret,
    {
      if (!setequal(names(ret), c("expr", "state"))) {
        stop("The block server for ", class(x)[1L],
             " is expected to return values `expr` and `state`.")
      }

      if (!is.reactive(ret[["expr"]])) {
        stop("The `expr` component of the return value for ", class(x)[1L],
             " is expected to be a reactive.")
      }

      expected <- union(block_ctor_inputs(x), block_ui_inputs(x))
      current <- names(ret[["state"]])

      if (!setequal(current, expected)) {
        stop("The `state` component of the return value for ", class(x)[1L],
             " is expected to return ",
             paste0("`", setdiff(expected, current), "`", collapse = ", "))
      }
    },
    once = TRUE
  )

  invisible()
}

#' Row-bind block constructor
#'
#' This block performs row-binding of data inputs.
#'
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_rbind_block <- function(...) {
  new_transform_block(
    function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {

          arg_names <- reactive(
            {
              dot_args <- names(...args)

              named <- !grepl("^[1-9][0-9]*$", dot_args)
              names(dot_args)[named] <- dot_args[named]

              dot_args
            }
          )

          list(
            expr = reactive(
              bquote(
                rbind(..(dat)),
                list(dat = lapply(arg_names(), as.name)),
                splice = TRUE
              )
            ),
            state = list()
          )
        }
      )
    },
    function(id) tagList(),
    dat_valid = function(...args) {
      stopifnot(length(...args) >= 1L)
    },
    allow_empty_state = TRUE,
    class = "rbind_block",
    ...
  )
}

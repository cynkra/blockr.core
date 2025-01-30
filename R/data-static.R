#' Data block constructor
#'
#' Mainly useful for testing and examples, this block simple returns the data
#' with which it was initialized.
#'
#' @param data Data (used directly as block result)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_static_block <- function(data, ...) {
  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(
              bquote(get("data", envir = .(env)), list(env = environment()))
            ),
            state = list(data = data)
          )
        }
      )
    },
    class = "static_block",
    ...
  )
}

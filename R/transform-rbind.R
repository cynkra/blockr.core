#' Row-bind block constructor
#'
#' This block performs row-binding of data inputs.
#'
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_rbind_block <- function(...) {
  new_transform_block(
    function(id, ...) {

      dot_args <- ...names()

      if (is.null(dot_args)) {
        dot_args <- paste0("...", seq_len(...length()))
      } else {
        names(dot_args) <- dot_args
      }

      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(
              bquote(
                rbind(..(dat)),
                list(dat = lapply(dot_args, as.name)),
                splice = TRUE
              )
            ),
            state = list()
          )
        }
      )
    },
    function(id) tagList(),
    dat_valid = function(...) {
      stopifnot(...length() >= 1L)
    },
    allow_empty_state = TRUE,
    class = "rbind_block",
    ...
  )
}

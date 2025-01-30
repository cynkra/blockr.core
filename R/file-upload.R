#' Data upload block constructor
#'
#' In order to make user data available to blockr, this block provides file-
#' upload functionality.
#'
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_upload_block <- function(...) {
  new_file_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(
              bquote(.(file), list(file = input$upload$datapath))
            ),
            state = list()
          )
        }
      )
    },
    function(id) {
      fileInput(
        NS(id, "upload"),
        "Upload data"
      )
    },
    class = "upload_block",
    ...
  )
}

#' @export
as.list.upload_block <- function(x, ...) {
  stop("Cannot serialize upload blocks.")
}

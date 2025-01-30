#' @rdname new_block
#' @export
new_file_block <- function(server, ui, class, ctor = sys.parent(), ...) {

  new_block(server, ui, c(class, "file_block"), ctor, ...)
}

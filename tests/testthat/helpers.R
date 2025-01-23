get_s3_method <- function(generic, object) {

  for (cls in class(object)) {
    res <- utils::getS3method(generic, cls, optional = TRUE)
    if (is.function(res)) {
      return(res)
    }
  }

  stop("No function found for generic `", generic, "()` and classes ",
       paste_enum(class(object)))
}

sink_msg <- function(...) {
  invisible(capture.output(..., type = "message"))
}

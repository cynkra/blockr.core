is_scalar <- function(x) length(x) == 1L

is_string <- function(x) {
  is.character(x) && is_scalar(x)
}

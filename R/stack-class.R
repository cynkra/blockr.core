#' Stacks
#'
#' Stacks are sets of blocks.
#'
#' @param blocks Set of blocks
#' @param id Unique stack ID
#' @param name Stack name
#' @param ... Extensibility
#' @param class (Optional) stack sub-class
#'
#' @export
new_stack <- function(blocks, id = rand_names(), name = NULL, ...,
                      class = character()) {

  if (is_blocks(blocks)) {
    blocks <- names(blocks)
  }

  stack_counter <- get_globals("stack_counter", session = NULL)

  if (is.null(name)) {
    name <- paste0("Stack ", stack_counter)
  }

  res <- validate_stack(
    new_vctr(blocks, id = id, name = name, ..., class = c(class, "stack"))
  )

  set_globals(stack_counter + 1L, "stack_counter", session = NULL)

  res
}

#' @param x Stack object
#' @rdname new_stack
#' @export
is_stack <- function(x) {
  inherits(x, "stack")
}

validate_stack <- function(x) {

  if (!is_stack(x)) {
    stop("Expecting a stack to inherit from `stack`.")
  }

  if (!is_string(attr(x, "id"))) {
    stop("Expecting the stack ID to be a string.")
  }

  if (!is_string(attr(x, "name"))) {
    stop("Expecting the stack name to be a string.")
  }

  if (!is.character(x)) {
    stop("Expecting the stack blocks to be strings.")
  }

  if (anyDuplicated(filter_empty(as.character(x))) != 0L) {
    stop("Stack blocks have to be unique.")
  }

  x
}

#' @export
vec_ptype_abbr.stack <- function(x, ...) {
  "stack"
}

#' @export
vec_restore.stack <- function(x, to, ...) {
  validate_stack(NextMethod())
}

#' @export
vec_ptype2.stack.stack <- function(x, y, ...) x

#' @export
vec_ptype2.character.stack <- function(x, y, ...) y

#' @export
vec_ptype2.stack.character <- function(x, y, ...) x

#' @export
vec_cast.stack.stack <- function(x, to, ...) x

#' @export
vec_cast.stack.character <- function(x, to, ...) as_stack(x)

#' @export
vec_cast.character.stack <- function(x, to, ...) vec_data(x)

#' @rdname new_stack
#' @export
as_stack <- function(x) UseMethod("as_stack")

#' @rdname new_stack
#' @export
as_stack.stack <- function(x) x

#' @rdname new_stack
#' @export
as_stack.character <- function(x) new_stack(x)

#' @importFrom generics intersect
#' @export
intersect.stack <- function(x, y, ...) {
  vec_set_intersect(x, y, ...)
}

#' @importFrom generics union
#' @export
union.stack <- function(x, y, ...) {
  vec_set_union(x, y, ...)
}

#' @importFrom generics setdiff
#' @export
setdiff.stack <- function(x, y, ...) {
  vec_set_difference(x, y, ...)
}

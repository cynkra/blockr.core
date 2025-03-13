reorder_rv <- function(x, new) {

  stopifnot(
    is.reactivevalues(x), setequal(new, names(x)), anyDuplicated(new) == 0L
  )

  internals <- .subset2(x, "impl")
  internals$.nameOrder <- new

  invisible(x)
}

make_read_only <- function(x) {

  stopifnot(is.reactivevalues(x))

  res <- unclass(x)
  res[["readonly"]] <- TRUE
  class(res) <- class(x)

  res
}

destroy_outputs <- function(ns_prefix, session = getDefaultReactiveDomain()) {

  for (id in starts_with(list_outputs(session), ns_prefix)) {
    destroy_output(id, session)
  }

  invisible()
}

list_outputs <- function(session = getDefaultReactiveDomain()) {
  coal(
    union(
      names(session$.__enclos_env__$private$.outputs),
      names(session$.__enclos_env__$private$.outputOptions)
    ),
    character()
  )
}

destroy_output <- function(id, session = getDefaultReactiveDomain()) {

  session$defineOutput(id, NULL, NULL)
  session$.__enclos_env__$private$.outputs[[id]] <- NULL
  session$.__enclos_env__$private$.outputOptions[[id]] <- NULL

  invisible()
}

destroy_inputs <- function(ns_prefix, session = getDefaultReactiveDomain()) {

  for (id in starts_with(names(session$input), ns_prefix)) {
    destroy_input(id, session)
  }

  invisible()
}

destroy_input <- function(id, session = getDefaultReactiveDomain()) {

  session$manageInputs(
    set_names(list(NULL), id),
    now = TRUE
  )

  input <- .subset2(session$input, "impl")

  input$.values$remove(id)
  input$.nameOrder <- setdiff(input$.nameOrder, id)

  invisible()
}

invalidate_inputs <- function(session = getDefaultReactiveDomain()) {

  input <- .subset2(session$input, "impl")

  input$.namesDeps$invalidate()
  input$.valuesDeps$invalidate()

  invisible()
}

destroy_observers <- function(x) {

  for (y in x) {
    destroy_observer(y)
  }

  invisible()
}

destroy_observer <- function(x) {

  if (is.null(x)) {
    return(invisible())
  }

  x$destroy()

  invisible()
}

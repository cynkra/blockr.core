blockr_globals_env <- list2env(
  list(
    stack_counter = 1L
  )
)

get_globals <- function(...) {

  get0(
    session_to_id(...),
    envir = blockr_globals_env,
    inherits = FALSE
  )
}

set_globals <- function(value, ...) {

  assign(
    session_to_id(...),
    value,
    envir = blockr_globals_env,
    inherits = FALSE
  )

  invisible()
}

session_to_id <- function(name = NULL, session = getDefaultReactiveDomain()) {

  if (is.null(session)) {
    return(name)
  }

  session$ns(name)
}

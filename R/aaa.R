blockr_globals_env <- list2env(
  list(
    stack_counter = 1L
  )
)

get_globals <- function(name = NULL, session = getDefaultReactiveDomain()) {

  get0(
    sess_to_name(session, name),
    envir = blockr_globals_env,
    inherits = FALSE
  )
}

set_globals <- function(value, name = NULL,
                        session = getDefaultReactiveDomain()) {

  assign(
    sess_to_name(session, name),
    value,
    envir = blockr_globals_env,
    inherits = FALSE
  )

  invisible()
}

sess_to_name <- function(sess, nme) {

  if (is.null(sess)) {
    return(nme)
  }

  sess$ns(nme)
}

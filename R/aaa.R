blockr_globals_env <- list2env(
  list(
    stack_counter = 1L,
    notification_ids = list()
  )
)

get_globals <- function(name) {
  get(name, envir = blockr_globals_env, inherits = FALSE)
}

set_globals <- function(name, value) {
  assign(name, value, envir = blockr_globals_env, inherits = FALSE)
  invisible()
}

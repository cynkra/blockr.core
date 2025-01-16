#' Blocks
#'
#' Blocks consist of an expression that defines what the block produces (given
#' the result of the previous block combined with user input), plus a class
#' attribute
#'
#' @param server A function returning [shiny::moduleServer()]
#' @param ui A function with a single argument (`ns`) returning a `shiny.tag`
#' @param class Block subclass
#' @param ctor Constructor name (or function/frame number)
#' @param ctor_pkg Package name (or `NULL`)
#' @param dat_val (Optioanl) input validator
#' @param uid Unique block ID
#' @param name Block name
#' @param ... Further (metadata) attributes
#'
#' @export
new_block <- function(server, ui, class, ctor, ctor_pkg, dat_val = NULL,
                      uid = rand_names(), name = NULL, ...) {

  stopifnot(
    is.character(class), length(class) > 0L, is_string(uid)
  )

  if (is.numeric(ctor)) {

    call <- deparse(sys.call(ctor)[[1L]])

    if (grepl("::", call, fixed = TRUE)) {

      call <- strsplit(call, "::", fixed = TRUE)[[1L]]

      stopifnot(length(call) == 2L)

      ctor <- call[2L]

      if (missing(ctor_pkg)) {
        ctor_pkg <- call[1L]
      } else {
        stopifnot(identical(ctor_pkg, call[1L]))
      }

    } else {

      if (missing(ctor_pkg)) {
        ctor_pkg <- utils::packageName(sys.frame(ctor))
      }

      ctor <- call
    }
  }

  if (is.null(ctor_pkg)) {
    stopifnot(is.function(ctor))
  } else {
    stopifnot(is_string(ctor), is_string(ctor_pkg))
  }

  if (is.null(name)) {
    name <- gsub("_", " ", class[1L])
    name <- paste0(toupper(substr(name, 1L, 1L)), substring(name, 2L))
  }

  validate_block(
    structure(
      list(
        expr_server = validate_block_server(server),
        expr_ui = validate_block_ui(ui),
        dat_val = validate_data_validator(dat_val, server)
      ),
      ...,
      uid = uid,
      ctor = ctor,
      ctor_pkg = ctor_pkg,
      name = name,
      class = c(class, "block")
    )
  )
}

validate_block_server <- function(server) {

  if (!is.function(server)) {
    stop("A block server component is expected to be a function.")
  }

  server
}

validate_block_ui <- function(ui) {

  if (!is.function(ui)) {
    stop("A block UI component is expected to be a function.")
  }

  if (!identical(names(formals(ui)), "ns")) {
    stop("A block UI function is expected to have a single argument `ns`.")
  }

  ui
}

validate_data_validator <- function(validator, server) {

  server_args <- names(formals(server))

  if (!length(server_args) && not_null(validator)) {
    stop("A nullary server function cannot accopmany a data input validator.")
  }

  if (not_null(validator)) {

    if (!is.function(validator)) {
      stop("Data input validator is expected to be a function.")
    }

    val_args <- names(formals(validator))

    if (!identical(server_args, val_args)) {
      stop("Block `", class[1L], "` has server args ", paste_enum(server_args),
           " which does not match validator args ", paste_enum(val_args), ".")
    }
  }

  validator
}

validate_block <- function(x) {

  if (!inherits(expr_ui(x), "shiny.tag", "shiny.tag.list", agg = any)) {
    stop("A block UI function is expected to return a shiny UI object, ",
         "i.e. either a `shiny.tag` or a `shiny.tag.list`.")
  }

  x
}

#' @param x An object inheriting from `"block"`
#' @rdname new_block
#' @export
is_block <- function(x) {
  inherits(x, "block")
}

#' @rdname new_block
#' @export
new_data_block <- function(server, ui, class, ctor = sys.parent(), ...) {

  new_block(server, ui, c(class, "data_block"), ctor, ...)
}

#' @rdname new_block
#' @export
new_transform_block <- function(server, ui, class,
                                ctor = sys.parent(), ...) {

  new_block(server, ui, c(class, "transform_block"), ctor, ...)
}

#' @rdname new_block
#' @export
block_uid <- function(x) {
  stopifnot(is_block(x))
  attr(x, "uid")
}

#' @rdname new_block
#' @export
block_name <- function(x) {
  stopifnot(is_block(x))
  attr(x, "name")
}

#' @rdname new_block
#' @export
block_ctor <- function(x) {

  stopifnot(is_block(x))

  fun <- attr(x, "ctor")

  if (is.function(fun)) {
    return(fun)
  }

  pkg <- attr(x, "ctor_pkg")

  get(fun, asNamespace(pkg), mode = "function")
}

#' @param namespace (Optional) parent namespace
#' @rdname new_block
#' @export
block_ns <- function(x, ..., namespace = NULL) {

  prefix <- paste(c(namespace, block_uid(x)), collapse = ns.sep)

  fun <- function(...) {

    if (...length() == 0L) {
      return(prefix)
    }

    id <- paste(c(...), collapse = ns.sep)

    if (length(prefix) == 0) {
      return(id)
    }

    paste(prefix, id, sep = ns.sep)
  }

  if (...length() == 0L) {
    return(fun)
  }

  fun(...)
}

#' @rdname new_block
#' @export
block_expr_server <- function(x) {
  x[["expr_server"]]
}

#' @rdname new_block
#' @export
block_expr_ui <- function(x) {
  x[["expr_ui"]]
}

#' @rdname new_block
#' @export
block_dat_val <- function(x) {
  x[["dat_val"]]
}

block_has_dat_val <- function(x) {
  not_null(block_dat_val(x))
}

#' @param data Data inputs
#' @rdname new_block
#' @export
validate_data_inputs <- function(x, data) {

  if (block_has_dat_val(x)) {
    return(do.call(block_dat_val(x), data))
  }

  NULL
}

#' @param data Data inputs
#' @rdname serve
#' @export
serve.block <- function(x, data, ...) {

  ui <- bslib::page_fluid(block_ui(x))

  server <- function(input, output, session) {
    block_server(x, lapply(data, reactiveVal))
  }

  shinyApp(ui, server)
}

#' @rdname serve
#' @export
serve.data_block <- function(x, ...) {
  NextMethod(data = list())
}

#' @rdname new_block
#' @export
block_inputs <- function(x) {
  UseMethod("block_inputs")
}

#' @rdname new_block
#' @export
block_inputs.block <- function(x) {
  names(formals(block_expr_server(x)))
}

block_ctor_inputs <- function(x) {
  setdiff(names(formals(block_ctor(x))), "...")
}

block_ui_inputs <- function(x) {
  setdiff(names(formals(block_expr_ui(x))), "ns")
}

#' @rdname new_block
#' @export
block_arity <- function(x) {

  args <- block_inputs(x)

  if ("..." %in% args) {
    return(NA_integer_)
  }

  length(args)
}

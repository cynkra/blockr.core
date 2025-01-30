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
#' @param dat_valid (Optioanl) input data validator
#' @param name Block name
#' @param allow_empty_state Either `TRUE`, `FALSE` or a character vector of
#' `state` values that may be empty while still moving forward with block eval
#' @param ... Further (metadata) attributes
#'
#' @export
new_block <- function(server, ui, class, ctor, ctor_pkg, dat_valid = NULL,
                      name = NULL, allow_empty_state = FALSE, ...) {

  stopifnot(is.character(class), length(class) > 0L)

  if (missing(ui)) {
    ui <- function(id) {
      tagList()
    }
  }

  if (is.numeric(ctor)) {

    fun <- sys.function(ctor)
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
        ctor_pkg <- utils::packageName(environment(fun))
      }

      if (not_null(ctor_pkg)) {
        ctor <- call
      }
    }

    if (is_string(ctor_pkg) && is_string(ctor)) {
      try <- get0(ctor, asNamespace(ctor_pkg), mode = "function",
                  inherits = FALSE)
    } else {
      try <- NULL
    }

    if (is.null(try)) {
      ctor <- fun
      ctor_pkg <- NULL
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
    new_vctr(
      list(
        expr_server = server,
        expr_ui = ui,
        dat_valid = dat_valid
      ),
      ...,
      ctor = ctor,
      ctor_pkg = ctor_pkg,
      name = name,
      allow_empty_state = allow_empty_state,
      class = c(class, "block")
    ),
    ui_eval = TRUE
  )
}

validate_block_server <- function(server) {

  if (!is.function(server)) {
    stop("A block server component is expected to be a function.")
  }

  args <- names(formals(server))

  if (!identical(args[1L], "id")) {
    stop(
      "A block server function is expected to have an argument `id` in ",
      "first poistion."
    )
  }

  if ("..." %in% args) {
    stop(
      "Variadic blocks are supported not by passing arguments as `...` ",
      "but using a special argument `...args`."
    )
  }

  invisible(server)
}

validate_block_ui <- function(ui) {

  if (!is.function(ui)) {
    stop("A block UI component is expected to be a function.")
  }

  if (!identical(names(formals(ui)), "id")) {
    stop("A block UI function is expected to have a single argument `id`.")
  }

  invisible(ui)
}

validate_data_validator <- function(validator, server) {

  server_args <- setdiff(names(formals(server)), "id")

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

  invisible(validator)
}

validate_block <- function(x, ui_eval = FALSE) {

  if (!is_block(x)) {
    stop("Expecting blocks to inherit from \"block\".")
  }

  if (class(x)[1L] == "block") {
    stop("Expecting blocks to have at least one sub-class.")
  }

  if (!is.list(x)) {
    stop("Expecting blocks to behave list-like.")
  }

  srv <- block_expr_server(x)

  validate_block_server(srv)
  validate_block_ui(block_expr_ui(x))
  validate_data_validator(block_dat_valid(x), srv)

  if (isTRUE(ui_eval)) {

    ui <- expr_ui("block", x)

    if (!inherits(ui, "shiny.tag", "shiny.tag.list", agg = any)) {
      stop("A block UI function is expected to return a shiny UI object, ",
           "i.e. either a `shiny.tag` or a `shiny.tag.list`.")
    }
  }

  if (!is.function(block_ctor(x))) {
    stop("Cannot reconstruct blocks without constructors.")
  }

  if (!is_string(block_name(x))) {
    stop("Expecting a string-valued block name.")
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
as_block <- function(x, ...) {
  UseMethod("as_block")
}

#' @rdname new_block
#' @export
as_block.block <- function(x, ...) {
  validate_block(x)
}

#' @rdname new_block
#' @export
as_block.list <- function(x, ...) {

  stopifnot(
    all(c("constructor", "payload", "package", "object") %in% names(x))
  )

  ctor <- get(
    x[["constructor"]],
    asNamespace(x[["package"]]),
    mode = "function"
  )

  args <- setdiff(names(formals(ctor)), "...")

  args <- c(
    x[["payload"]][args],
    ctor = x[["constructor"]],
    ctor_pkg = x[["package"]]
  )

  res <- do.call(ctor, args)

  if (!identical(class(res), x[["object"]])) {
    stop("Could not deserialize block.")
  }

  res
}

#' @export
as.list.block <- function(x, state = NULL, ...) {

  pkg <- attr(x, "ctor_pkg")

  if (is.null(state)) {
    state <- initial_block_state(x)
  }

  attrs <- attributes(x)

  state <- c(
    state,
    attrs[setdiff(names(attrs), c("names", "ctor", "ctor_pkg", "class"))]
  )

  list(
    object = class(x),
    payload = state,
    constructor = attr(x, "ctor"),
    package = pkg,
    version = as.character(utils::packageVersion(pkg))
  )
}

#' @export
c.block <- function(...) {
  as_blocks(lapply(list(...), as_block))
}

#' @export
vec_restore.block <- function(x, to, ...) {
  validate_block(NextMethod())
}

#' @export
vec_ptype2.block.block <- function(x, y, ...) x

#' @export
vec_ptype2.list.block <- function(x, y, ...) y

#' @export
vec_ptype2.block.list <- function(x, y, ...) x

#' @export
vec_cast.block.block <- function(x, to, ...) x

#' @export
vec_cast.block.list <- function(x, to, ...) as_block(x)

#' @export
vec_cast.list.block <- function(x, to, ...) as.list(x)

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

  get(fun, envir = asNamespace(pkg), mode = "function", inherits = FALSE)
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
block_dat_valid <- function(x) {
  x[["dat_valid"]]
}

#' @rdname new_block
#' @export
block_has_data_validator <- function(x) {
  not_null(block_dat_valid(x))
}

block_allow_empty_state <- function(x) {
  attr(x, "allow_empty_state")
}

#' @param data Data inputs
#' @rdname new_block
#' @export
validate_data_inputs <- function(x, data) {

  if (block_has_data_validator(x)) {
    return(do.call(block_dat_valid(x), data))
  }

  NULL
}

#' @param data Data inputs
#' @param id Block ID
#' @rdname serve
#' @export
serve.block <- function(x, data, id = "block", ...) {

  ui <- bslib::page_fluid(block_ui(id, x))

  server <- function(input, output, session) {
    block_server(id, x, lapply(data, reactiveVal))
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
block_inputs <- function(x, ...) {
  UseMethod("block_inputs")
}

#' @rdname new_block
#' @export
block_inputs.block <- function(x, ...) {
  setdiff(block_expr_inputs(x), "...args")
}

block_expr_inputs <- function(x) {
  setdiff(names(formals(block_expr_server(x))), "id")
}

block_ctor_inputs <- function(x) {
  setdiff(names(formals(block_ctor(x))), "...")
}

initial_block_state <- function(x) {
  lapply(
    set_names(nm = block_ctor_inputs(x)),
    get,
    envir = environment(block_expr_server(x))
  )
}

#' @rdname new_block
#' @export
block_arity <- function(x) {

  if (is_board(x)) {
    return(
      set_names(int_ply(board_blocks(x), block_arity), board_block_ids(x))
    )
  }

  args <- block_expr_inputs(x)

  if ("...args" %in% args) {
    return(NA_integer_)
  }

  length(args)
}

#' @export
format.block <- function(x, ...) {

  out <- ""

  for (cl in rev(setdiff(class(x), c("list", "vctrs_vctr")))) {
    out <- paste0("<", cl, out, ">")
  }

  out <- c(
    out,
    paste0("Name: \"", attr(x, "name"), "\"")
  )

  arity <- block_arity(x)

  if (is.na(arity)) {
    dat <- "Indefinite arity"
  } else if (arity > 0L) {
    dat <- paste0("Data inputs: ", paste_enum(block_inputs(x), quotes = "\""))
  } else {
    dat <- "No data inputs"
  }

  out <- c(out, dat)

  if (length(block_ctor_inputs(x))) {

    args <- initial_block_state(x)
    args <- trimws(utils::capture.output(utils::str(args))[-1L], "right")

    out <- c(out, "Initial block state:", args)

  } else {

    out <- c(out, "Stateless block")
  }

  c(
    out,
    paste0("Constructor: ", attr(x, "ctor_pkg"), "::", attr(x, "ctor"), "()")
  )
}

#' @export
print.block <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

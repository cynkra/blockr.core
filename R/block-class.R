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
        ctor_pkg <- pkg_name(environment(fun))
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
    abort(
      "A block server component is expected to be a function.",
      class = "block_server_function_invalid"
    )
  }

  args <- names(formals(server))

  if (!identical(args[1L], "id")) {
    abort(
      paste(
        "A block server function is expected to have an argument `id` in",
        "first poistion."
      ),
      class = "block_server_arg_id_invalid"
    )
  }

  if ("..." %in% args) {
    abort(
      paste(
        "Variadic blocks are supported not by passing arguments as `...`",
        "but using a special argument `...args`."
      ),
      class = "block_server_dots_invalid"
    )
  }

  if (any(grepl("^[1-9][0-9]*$", args))) {
    abort(
      paste(
        "Integer-valued argument names are reserved as poistional arguments",
        "in `...args.`"
      ),
      class = "block_server_args_invalid"
    )
  }

  invisible(server)
}

validate_block_ui <- function(ui) {

  if (!is.function(ui)) {
    abort(
      "A block UI component is expected to be a function.",
      class = "block_ui_function_invalid"
    )
  }

  if (!identical(names(formals(ui)), "id")) {
    abort(
      "A block UI function is expected to have a single argument `id`.",
      class = "block_ui_arg_id_invalid"
    )
  }

  invisible(ui)
}

validate_data_validator <- function(validator, server) {

  server_args <- setdiff(names(formals(server)), "id")

  if (!length(server_args) && not_null(validator)) {
    abort(
      "A nullary server function cannot accopmany a data input validator.",
      class = "block_validator_nullary_invalid"
    )
  }

  if (not_null(validator)) {

    if (!is.function(validator)) {
      abort(
        "Data input validator is expected to be a function.",
        class = "block_validator_function_invalid"
      )
    }

    val_args <- names(formals(validator))

    if (!identical(server_args, val_args)) {
      abort(
        paste0(
          "Server args ", paste_enum(server_args), " do not match validator ",
          "args ", paste_enum(val_args), "."
        ),
        class = "block_validator_args_invalid"
      )
    }
  }

  invisible(validator)
}

validate_block <- function(x, ui_eval = FALSE) {

  if (!is_block(x)) {
    abort(
      "Expecting blocks to inherit from \"block\".",
      class = "block_class_invalid"
    )
  }

  if (class(x)[1L] == "block") {
    abort(
      "Expecting blocks to have at least one sub-class.",
      class = "block_class_invalid"
    )
  }

  if (!is.list(x)) {
    abort(
      "Expecting blocks to behave list-like.",
      class = "block_list_like_invalid"
    )
  }

  if (!is.function(try(block_ctor(x), silent = TRUE))) {
    abort(
      "Cannot reconstruct blocks without constructors.",
      class = "block_no_ctor"
    )
  }

  if (!is_string(block_name(x))) {
    abort(
      "Expecting a string-valued block name.",
      class = "block_name_invalid"
    )
  }

  srv <- block_expr_server(x)

  validate_block_server(srv)
  validate_block_ui(block_expr_ui(x))
  validate_data_validator(block_dat_valid(x), srv)

  if (isTRUE(ui_eval)) {

    ui <- expr_ui("block", x)

    if (!inherits(ui, "shiny.tag", "shiny.tag.list", agg = any)) {
      abort(
        paste(
          "A block UI function is expected to return a shiny UI object,",
          "i.e. either a `shiny.tag` or a `shiny.tag.list`."
        ),
        class = "block_ui_eval_invalid"
      )
    }
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
    version = as.character(pkg_version(pkg))
  )
}

#' @export
c.block <- function(...) {
  as_blocks(lapply(list(...), as_block))
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

#' @param id Block ID
#' @param data Data inputs
#' @rdname serve
#' @export
serve.block <- function(x, id = "block", ..., data = list()) {

  init_data <- function(x, is_variadic) {
    if (is_variadic) do.call(reactiveValues, x) else reactiveVal(x)
  }

  if (...length() && !length(data)) {
    data <- list(...)
  }

  dot_args <- !names(data) %in% block_inputs(x)

  if (!is.na(block_arity(x)) && any(dot_args)) {
    stop("Unexpected arguments.")
  }

  if (any(dot_args)) {
    data <- c(data[!dot_args], list(...args = data[dot_args]))
  }

  if ("...args" %in% names(data)) {

    arg_names <- names(data[["...args"]])
    pos_args <- grepl("[1-9][0-9]*", arg_names)

    pos <- which(pos_args)
    ind <- c(pos[order(as.integer(arg_names[pos]))], which(!pos_args))

    data[["...args"]] <- data[["...args"]][ind]
  }

  ui <- bslib::page_fluid(block_ui(id, x))

  server <- function(input, output, session) {

    res <- block_server(id, x, Map(init_data, data, names(data) == "...args"))

    exportTestValues(
      result = safely_export(res$result())()
    )

    invisible()
  }

  shinyApp(ui, server)
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

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

  args <- block_inputs(x)

  if ("..." %in% args) {
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

  if (block_arity(x)) {
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

#' @rdname new_block
#' @export
blocks <- function(...) {

  args <- list(...)

  args <- lapply(args, as_block)

  if (is.null(names(args))) {
    names(args) <- rand_names(n = length(args))
  }

  miss <- is.na(names(args)) | names(args) == ""

  if (any(miss)) {
    names(args)[miss] <- rand_names(names(args)[!miss], sum(miss))
  }

  validate_blocks(
    new_vctr(args, class = "blocks")
  )
}

#' @rdname new_block
#' @export
is_blocks <- function(x) {
  inherits(x, "blocks")
}

#' @rdname new_block
#' @export
as_blocks <- function(x, ...) {
  UseMethod("as_blocks")
}

#' @rdname new_block
#' @export
as_blocks.blocks <- function(x, ...) {
  validate_blocks(x)
}

#' @rdname new_block
#' @export
as_blocks.list <- function(x, ...) {
  do.call(blocks, x)
}

#' @rdname new_block
#' @export
as_blocks.block <- function(x, ...) {
  as_blocks(list(x))
}

#' @export
as.list.blocks <- function(x, ...) {
  vec_data(x)
}

#' @export
`names<-.blocks` <- function(x, value) {

  if (is.null(value)) {
    value <- rep("", length(x))
  } else if (anyDuplicated(value) != 0L) {
    stop("IDs are required to be unique.")
  }

  names(x) <- value

  x
}

#' @export
format.blocks <- function(x, ...) {

  out <- lapply(x, format)
  out <- Map(c, names(x), lapply(out, function(x) paste0(" ", x)))
  out <- lapply(out, c, "")
  out <- unlst(out)

  c(
    paste0("<", class(x)[1L], "[", length(x), "]>"),
    if (length(x)) "",
    out[-length(out)]
  )
}

#' @export
print.blocks <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
vec_restore.blocks <- function(x, to, ...) {
  validate_blocks(NextMethod())
}

#' @export
vec_ptype2.blocks.blocks <- function(x, y, ...) x

#' @export
vec_ptype2.list.blocks <- function(x, y, ...) y

#' @export
vec_ptype2.blocks.list <- function(x, y, ...) x

#' @export
vec_ptype2.block.blocks <- function(x, y, ...) y

#' @export
vec_ptype2.blocks.block <- function(x, y, ...) x

#' @export
vec_cast.blocks.blocks <- function(x, to, ...) x

#' @export
vec_cast.blocks.list <- function(x, to, ...) as_blocks(x)

#' @export
vec_cast.list.blocks <- function(x, to, ...) as.list(x)

#' @export
vec_cast.blocks.block <- function(x, to, ...) as_blocks(x)

#' @export
c.blocks <- function(...) {

  args <- lapply(list(...), as_blocks)
  args <- lapply(args, as.list)
  args <- do.call(c, args)

  validate_blocks(
    do.call(c, args)
  )
}

#' @export
`[<-.blocks` <- function(x, i, ..., value) {

  i <- vec_as_location(i, length(x), names(x))

  if (is.null(value)) {
    return(blocks_slice(x, -i))
  }

  value <- as_blocks(value)

  trg_ids <- names(x)[i]

  if (is.null(names(value))) {
    names(value) <- trg_ids
  }

  new_ids <- names(value)

  if (!setequal(new_ids, trg_ids)) {
    stop(
      "Replacing IDs ", paste_enum(trg_ids), " with ", paste_enum(new_ids),
      " is not allowed."
    )
  }

  blocks_assign(x, i, value[trg_ids])
}

blocks_slice <- function(...) {
  validate_blocks(vec_slice(...))
}

blocks_assign <- function(...) {
  validate_blocks(vec_assign(...))
}

validate_blocks <- function(x) {

  if (!is_blocks(x)) {
    stop("Expecting blocks to inherit from \"blocks\".")
  }

  if (!is.list(x) || !all(lgl_ply(x, is_block))) {
    stop("Expecting the board to contain a set of blocks.")
  }

  ids <- names(x)

  if (length(ids) != length(x) || any(is.na(ids) | !nchar(ids))) {
    stop("Block IDs are required to be nonempty strings.")
  }

  if (anyDuplicated(ids) != 0) {
    stop("Block IDs are required to be unique.")
  }

  x
}

#' Board
#'
#' Blocks are placed on a board.
#'
#' @param blocks Set of blocks
#' @param links Set of links
#' @param stacks Set of stacks
#' @param options Board-level user settings
#' @param ... Further (metadata) attributes
#' @param class Board sub-class
#'
#' @export
new_board <- function(blocks = list(), links = list(), stacks = list(),
                      options = new_board_options(), ...,
                      class = character()) {

  blocks <- as_blocks(blocks)
  links <- as_links(links)
  stacks <- as_stacks(stacks)
  options <- as_board_options(options)

  links <- complete_unary_inputs(links, blocks)
  links <- complete_variadic_inputs(links, blocks)

  validate_board(
    structure(
      list(
        blocks = blocks,
        links = links,
        stacks = stacks,
        options = options,
        ...
      ),
      class = c(class, "board")
    )
  )
}

complete_unary_inputs <- function(x, blocks) {

  ids <- names(blocks)

  to_complete <- x$to %in% ids[int_ply(blocks, block_arity) == 1L] & (
    is.na(x$input) | nchar(x$input) == 0L
  )

  inputs <- lapply(blocks[unique(x$to[to_complete])], block_inputs)

  x$input[to_complete] <- chr_ply(inputs[x$to[to_complete]], identity)

  x
}

complete_variadic_inputs <- function(x, blocks) {

  ids <- names(blocks)

  to_complete <- x$to %in% ids[is.na(int_ply(blocks, block_arity))] & (
    is.na(x$input) | nchar(x$input) == 0L
  )

  to_todo <- x$to[to_complete]

  x$input[to_complete] <- stats::ave(to_todo, to_todo, FUN = seq_along)

  x
}

validate_board_blocks_links <- function(blocks, links) {

  ids <- names(blocks)

  if (!all(links$from %in% ids) || !all(links$to %in% ids)) {
    abort(
      "Expecting all links to refer to known block IDs.",
      class = "board_block_link_name_mismatch"
    )
  }

  arity <- int_ply(blocks, block_arity)

  for (i in Filter(Negate(is.na), unique(arity))) {

    fail <- table(factor(links$to, levels = ids[arity == i])) > i

    if (any(fail)) {
      abort(
        paste0(
          i, "-ary blocks are expected to have at most ", i, " incoming ",
          "edge(s), which does not hold for block(s) ",
          paste_enum(names(fail)[fail]), "."
        ),
        class = "board_block_link_arity_mismatch"
      )
    }
  }

  inputs <- lapply(blocks[!is.na(arity)], block_inputs)

  for (i in intersect(links$to, names(inputs))) {

    unknown <- setdiff(links$input[links$to == i], inputs[[i]])

    if (length(unknown)) {
      abort(
        paste0(
          "Block ", i, " expects inputs ", paste_enum(inputs[[i]]),
          " but received ", paste_enum(unknown)
        ),
        class = "board_block_link_input_mismatch"
      )
    }
  }

  invisible()
}

validate_board_blocks_stacks <- function(blocks, stacks) {

  for (i in seq_along(stacks)) {

    stk <- stacks[[i]]
    chk <- is.element(stk, names(blocks))

    if (!all(chk)) {
      abort(
        paste0(
          "Unknown block", if (sum(!chk) > 1L) "s" else "", " ",
          paste_enum(stk[!chk]), " are assigned to stack ", names(stacks)[i],
          "."
        ),
        class = "board_block_stack_name_mismatch"
      )
    }
  }

  invisible()
}

#' @param x Board object
#' @rdname new_board
#' @export
validate_board <- function(x) {

  if (!is_board(x)) {
    abort(
      "Expecting a board object to inherit from \"baord\".",
      class = "board_inheritance_invalid"
    )
  }

  if (!is.list(x)) {
    abort(
      "Expecting a board object to be list-like.",
      class = "board_list_like_invalid"
    )
  }

  cmps <- c("blocks", "links")

  if (!all(cmps %in% names(x))) {
    abort(
      "Expecting a board object to contain components ", paste_enum(cmps), ".",
      class = "board_list_components_invalid"
    )
  }

  blks <- board_blocks(x)

  validate_board_blocks_links(blks, board_links(x))
  validate_board_blocks_stacks(blks, board_stacks(x))

  x
}

#' @rdname new_board
#' @export
is_board <- function(x) {
  inherits(x, "board")
}

#' @export
sort.board <- function(x, decreasing = FALSE, ...) {

  res <- topo_sort(as.matrix(x))
  ids <- board_block_ids(x)

  ind <- match(res, ids)

  if (isTRUE(decreasing)) {
    ind <- rev(ind)
  }

  board_blocks(x) <- board_blocks(x)[ind]

  x
}

#' @export
as.matrix.board <- function(x, ...) {

  block_ids <- board_block_ids(x)
  links <- board_links(x)

  as_adjacency_matrix(links$from, links$to, block_ids)
}

#' @rdname topo_sort
#' @export
is_acyclic.board <- function(x) {
  is_acyclic(as.matrix(x))
}

#' @param id Board namespace ID
#' @param plugins Board plugins
#' @rdname serve
#' @export
serve.board <- function(x, id = rand_names(), plugins = borad_plugins(), ...) {

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    title = board_option("board_name", x),
    board_ui(id, x, plugins),
    htmltools::htmlDependency(
      "change-board-title",
      pkg_version(),
      src = pkg_file("assets", "js"),
      script = "changeBoardTitle.js"
    )
  )

  server <- function(input, output, session) {

    observeEvent(
      board_option_from_userdata("board_name", session),
      session$sendCustomMessage(
        "change-board-title",
        board_option_from_userdata("board_name", session)
      )
    )

    res <- board_server(id, x, plugins)

    exportTestValues(
      result = lapply(
        lapply(
          lapply(lst_xtr(res[[1L]]$blocks, "server", "result"), safely_export),
          reval
        ),
        reval
      )
    )

    invisible()
  }

  shinyApp(ui, server)
}

#' @rdname new_board
#' @export
board_blocks <- function(x) {
  stopifnot(is_board(x))
  validate_blocks(x[["blocks"]])
}

#' @param value Replacement value
#' @rdname new_board
#' @export
`board_blocks<-` <- function(x, value) {
  stopifnot(is_board(x))
  x[["blocks"]] <- value
  validate_board(x)
}

#' @rdname new_board
#' @export
board_links <- function(x) {
  stopifnot(is_board(x))
  validate_links(x[["links"]])
}

#' @rdname new_board
#' @export
`board_links<-` <- function(x, value) {
  stopifnot(is_board(x))
  x[["links"]] <- value
  validate_board(x)
}

#' @rdname new_board
#' @export
board_stacks <- function(x) {
  stopifnot(is_board(x))
  validate_stacks(x[["stacks"]])
}

#' @rdname new_board
#' @export
`board_stacks<-` <- function(x, value) {
  stopifnot(is_board(x))
  x[["stacks"]] <- value
  validate_board(x)
}

#' @rdname new_board
#' @export
board_options <- function(x) {

  if (!inherits(x, "board")) {
    abort(
      "Can only extract board options from a board object.",
      class = "board_options_board_invalid"
    )
  }

  validate_board_options(x[["options"]])
}

#' @rdname new_board
#' @export
board_block_ids <- function(x) {
  names(board_blocks(x))
}

#' @rdname new_board
#' @export
board_link_ids <- function(x) {
  names(board_links(x))
}

#' @rdname new_board
#' @export
board_stack_ids <- function(x) {
  names(board_stacks(x))
}

#' @rdname new_board
#' @export
available_stack_blocks <- function(x, stacks = board_stacks(x),
                                   blocks = board_stack_ids(x)) {

  Reduce(setdiff, lapply(stacks, as.character), blocks)
}

#' @param add Links/stacks to add
#' @param rm Link/stack IDs to remove
#' @param mod Link/stack IDs to modify
#' @rdname new_board
#' @export
modify_links <- function(x, add = NULL, rm = NULL, mod = NULL) {

  links <- board_links(x)

  if (is_links(rm)) {
    rm <- names(rm)
  }

  if (length(rm)) {
    stopifnot(is.character(rm), all(rm %in% names(links)))
    links <- links[!names(links) %in% rm]
  }

  if (length(mod)) {
    links[names(mod)] <- mod
  }

  board_links(x) <- c(links, add)

  x
}

#' @rdname new_board
#' @export
modify_stacks <- function(x, add = NULL, rm = NULL, mod = NULL) {

  stacks <- board_stacks(x)

  if (is_stacks(rm)) {
    rm <- names(rm)
  }

  if (length(rm)) {
    stopifnot(is.character(rm), all(rm %in% names(stacks)))
    stacks <- stacks[!names(stacks) %in% rm]
  }

  if (length(mod)) {
    stacks[names(mod)] <- mod
  }

  board_stacks(x) <- c(stacks, add)

  x
}

#' @rdname new_board
#' @export
rm_blocks <- function(x, rm) {

  if (is_blocks(rm)) {
    rm <- names(rm)
  }

  blocks <- board_blocks(x)

  stopifnot(is.character(rm), all(rm %in% names(blocks)))

  links <- board_links(x)
  board_links(x) <- links[!links$from %in% rm & !links$to %in% rm]

  stacks <- board_stacks(x)
  board_stacks(x) <- as_stacks(lapply(stacks, setdiff, rm))

  board_blocks(x) <- blocks[!names(blocks) %in% rm]

  x
}

#' @rdname new_block
#' @export
block_inputs.board <- function(x, ...) {
  lapply(set_names(board_blocks(x), board_block_ids(x)), block_inputs)
}

#' @export
format.board <- function(x, ...) {

  paste_name_append_empty <- function(x, nme) {
    c(paste0(nme, x[1L]), x[-1L], "")
  }

  out <- ""

  for (cl in rev(class(x))) {
    out <- paste0("<", cl, out, ">")
  }

  blk <- board_blocks(x)

  if (length(blk)) {

    blk <- lapply(blk, format, ...)
    blk <- map(paste_name_append_empty, blk, names(blk))

    out <- c(
      out,
      "",
      paste0("Blocks[", length(blk), "]:"),
      "",
      unlst(blk)
    )
  }

  lnk <- board_links(x)

  if (length(lnk)) {

    out <- c(
      out,
      paste0("Links[", length(lnk), "]:"),
      "",
      paste0(names(lnk), ": ", format(lnk))
    )
  }

  stk <- board_stacks(x)

  if (length(stk)) {

    stk <- lapply(stk, format, ...)
    stk <- map(paste_name_append_empty, stk, names(stk))

    out <- c(
      out,
      if (length(lnk)) "",
      paste0("Stacks[", length(stk), "]:"),
      "",
      unlst(stk)
    )
  }

  if ((length(blk) && !length(lnk)) || length(stk)) {
    out <- out[-length(out)]
  }

  c(
    out,
    "",
    "Options:",
    format(board_options(x))
  )
}

#' @export
print.board <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

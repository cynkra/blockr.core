
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blockr.core

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![status](https://github.com/cynkra/blockr.core/actions/workflows/ci.yaml/badge.svg)](https://github.com/cynkra/blockr.core/actions/workflows/ci.yaml)
[![coverage](https://codecov.io/gh/cynkra/blockr.core/graph/badge.svg?token=VoOPRU65KA)](https://codecov.io/gh/cynkra/blockr.core)
<!-- badges: end -->

Designed to democratize data analysis, `blockr.core` provides a
flexible, intuitive, and **code-free** approach to building data
pipelines.

## Installation

You can install the development version of blockr.core from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cynkra/blockr.core")
```

## Example

A single block server instance can be spun up as

``` r
library(blockr.core)
serve(new_dataset_block("iris"))
```

or for a block that requires input, by additionally passing static data
such as

``` r
serve(
  new_merge_block(by = "name"),
  data = list(x = datasets::BOD, y = datasets::ChickWeight)
)
```

If previewing multiple connected block is desired, a board can be
created and passed to `serve()`, e.g.

``` r
serve(
  new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("ChickWeight"),
      c = new_merge_block("Time")
    ),
    links = links(from = c("a", "b"), to = c("c", "c"), input = c("x", "y"))
  )
)
```

## How to create a block

A block constructor should expose as arguments anything that defines its
*state*, i.e. things the user might set via UI which are not derived
from input data. For example in a `shiny::selectInput()`, where the
`choices` are data columns, this should not be a constructor argument
(as this value will be computed from the data), whereas the `selected`
value should be exposed.

The constructor return value should be a call to `new_block()` (or if
applicable a call to the more specific *virtual* constructors
`new_data_block()`, `new_transform_block()`, etc.). Arguments `server`
and `ui` both expect closures with a specific structure.

### Block `server`

- A signature with as many arguments as data inputs is expected: zero
  for a data block, one for a transform block such as a select block,
  two for a join block and the special argument `...args` for variadic
  blocks, such as an `rbind` block (all, excluding the required first
  argument `id`).
- The first argument `id` should be passed to the
  `shiny::moduleServer()` call.
- As return value a call to `shiny::moduleServer()` is expected,
  containing a `module` function that in turn returns a list with
  entries `expr` and `state`.
- The block expression is a quoted reactive value that contains
  user-supplied components and is updated whenever user values change.
  Data names should match between expressio and the top-level function
  arguments.
- Block state is defined by the `state` entry which again is a list of
  reactives. The set of returned values should match (both in count and
  names) that of the constructor signature.

The `server` component of an identity transform block could be formed as

``` r
function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      list(
        expr = reactive(quote(identity(data))),
        state = list()
      )
    }
  )
}
```

Note that `data` is the same name in the expression and the top-level
function signature.

### Block `ui`

- The function signature is expected to contain a single `id` argument,
  which can be used with `shiny::NS()` to construct namespaced IDs.
- A call to appropriate shiny UI functions is expected that return
  `shiny.tag` or `shiny.tag.list` objects.
- The initial evaluation of the `ui` function will be performed in the
  context of the constructor scope (i.e. if a value of name `xyz` is
  bound the the constructor scope or a parent thereof, this value will
  be passed as `xyz` argument).

The `ui` component of an identity transform block is trivial:

``` r
function(id) {
  tagList()
}
```

Putting this together, a dataset block could be constructed as

``` r
new_dataset_block <- function(dataset = character(), package = "datasets",
                              ...) {

  envir <- as.environment(paste0("package:", package))

  choices <- ls(envir = envir)
  choices <- choices[
    vapply(mget(choices, envir = envir), is.data.frame, logical(1L))
  ]

  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {

          dat <- reactiveVal(dataset)

          observeEvent(input$dataset, dat(input$dataset))

          list(
            expr = reactive(
              bquote(
                `::`(.(pkg), .(dat)),
                list(pkg = package, dat = dat())
              )
            ),
            state = list(
              dataset = reactive(input$dataset),
              package = package
            )
          )
        }
      )
    },
    function(id) {
      selectInput(
        inputId = NS(id, "dataset"),
        label = "Dataset",
        choices = choices,
        selected = dataset
      )
    },
    class = "dataset_block",
    ...
  )
}
```

Note that both `server` and `ui` are closures and therfore may refer to
names bound in the constructor scope (e.g. `dataset` and `choices`). The
`state` entry returned by the server module contains the static value
`package` and this is needed for ser/deser (alongside the dynamic value
`dataset`).

The `expr` return value is here constructed via a call to
`base::bquote()`. Many other options exsist, such as using tools offered
by the rlang package or by passing a string to `base::parse()`, e.g.

``` r
list(
  expr = reactive(parse(text = paste0(package, "::", dataset))[[1]]),
  ...
)
```

The block can then be manipulated via UI as

``` r
serve(new_dataset_block("iris"))
```

Another example is a `utils::head()` block, such as the one offered as
`new_head_block()`:

``` r
new_head_block <- function(n = 6L, ...) {

  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          n_rows <- reactiveVal(n)

          observeEvent(input$n, n_rows(input$n))

          observeEvent(
            nrow(data()),
            updateNumericInput(
              inputId = "n",
              value = n_rows(),
              min = 1L,
              max = nrow(data())
            )
          )

          list(
            expr = reactive(
              bquote(utils::head(data, n = .(n)), list(n = n_rows()))
            ),
            state = list(
              n = n_rows
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        numericInput(
          inputId = NS(id, "n"),
          label = "Number of rows",
          value = n,
          min = 1L
        )
      )
    },
    dat_val = function(data) {
      stopifnot(is.data.frame(data) || is.matrix(data))
    },
    class = "head_block",
    ...
  )
}
```

Here, the state return value is a list of length 1, containing the
current value for the only constructor argument. The current value for
`n` is represented by a `shiny::reactiveVal()`, `n_rows()`, which is
initialized with the values `n` in the constructor scope and updated on
every change to `input$n`. Furthermore the `max` value for the
`shiny::numericInput()` field is updated on every change to the number
of data rows.

A final `block` constructor argument of note might be `dat_val`, which
is an optional function that can be passed which signals to the
framework, if data passed to a block, can actually be processed by the
block.

An app containing such a head block can be spun up as

``` r
serve(new_head_block(n = 10L), list(data = mtcars))
```

For an example with multiple data inputs, refer to examples such as
`new_merge_block()`. Such a binary block (with arguments `x` and `y`)
can be explored in a standalone app (with nonsensical inputs) as

``` r
serve(
  new_merge_block(by = "Time"),
  data = list(x = datasets::BOD, y = datasets::ChickWeight)
)
```

The `data` argument to `serve()` expects a list with names components
that match the server function signature, i.e. `data` for
`new_head_block()` and `x`, `y` for `new_merge_block()`. Such names can
be chosen freely by the block implementer (with the exception of
integer-valued names, such as `` `1` ``, `` `2` ``, etc. which are
reserved as positional arguments in `...args`).

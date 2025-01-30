
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
      a = new_dataset_block(),
      b = new_dataset_block(),
      c = new_rbind_block()
    ),
    links = links(from = "a", to = "c")
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
  two for a join block and `...` for an `rbind` block (excluding the
  required first argument `id`).
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

Another example is a `dplyr::select()` block, such as the one offered by
the blockr.dplyr extension package:

``` r
new_select_block <- function(columns = character(), ...) {

  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          sels <- reactiveVal(columns)
          cols <- reactive(colnames(data()))

          observe(
            {
              sels(intersect(input$columns, cols()))

              updateSelectInput(
                session,
                inputId = "columns",
                choices = cols(),
                selected = sels()
              )
            }
          )

          list(
            expr = reactive(
              bquote(
                dplyr::select(data, ..(cols)),
                list(cols = lapply(sels(), as.name)),
                splice = TRUE
              )
            ),
            state = list(
              columns = reactive(sels()),
              choices = reactive(cols())
            )
          )
        }
      )
    },
    function(id) {
      selectInput(
        inputId = NS(id, "columns"),
        label = "Columns",
        choices = list(),
        multiple = TRUE
      )
    },
    class = "select_block",
    ...
  )
}
```

Here, the state return value contains entries `columns` (required by the
constructor) and `choices`, which is not an argument of the constructor,
but an argument of the UI function. The value for `choices` can be
inferred by the server function but is not available when first drawing
the UI. Consequently the signature of the UI function requires a default
value (for the initial UI rendering) and the server function contains a
call to `shiny::updateSelectInput()` to populate the selec choices as
soon as the corresponding value is available. As the UI function can be
used to re-draw the UI from scratch, `choices` also is required to be
included in `state` for correctly re-drawing the block UI.

An app containing such a select block can be spun up as

``` r
serve(new_select_block(c("mpg", "cyl")), list(data = mtcars))
```

For an example with multiple data inputs, refer to
`blockr.dplyr::new_join_block()`, which can be explored in a standalone
app as

``` r
serve(
  blockr.dplyr::new_join_block(by = "name"),
  data = list(x = dplyr::band_members, y = dplyr::band_instruments)
)
```

The `data` argument to `serve()` expects a list with names components
that match the server function signature, i.e. `data` for
`blockr.core::new_select_block()` and `x`, `y` for
`blockr.dplyr::new_join_block()`. These names can be chosen freely by
the block implementer.

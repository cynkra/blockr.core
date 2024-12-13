
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blockr.core

<!-- badges: start -->

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

## How to create a block

A block constructor should expose as arguments anything that defines its
*state*, i.e. things the user might set via UI which are not derived
from input data. For example in a `selectInput()`, where the `choices`
are data columns, this should not be a constructor argument (as this
value will be computed from the data), whereas the `selected` value
should be exposed.

The constructor return value should be a call to `new_block()` (or if
applicable a call to the more specific *virtual* constructors
`new_data_block()`, `new_transform_block()`, etc.). Arguments `server`
and `ui` both expect closures with a specific structure.

### Block `server`

- A signature with as many arguments as data inputs is expected: zero
  for a data block, one for a transform block such as a select block,
  two for a join block and `...` for an `rbind` block.
- As return value a call to `moduleServer()` is expected, containing a
  `module` function that in turn returns a list with entries `expr` and
  `state`.
- The block expression is a quoted reactive value that contains
  user-supplied components and is updated whenever user values change.
- Block state is defined by the `state` entry which again is a list of
  reactives. This is a superset of the constructor arguments (and
  variable names should be aligned such that the constructor can be
  called again with an appropriately subsetted state).

The `server` component of an identity transform block could be formed as

``` r
function(data) {
  moduleServer(
    "expression",
    function(input, output, session) {
      list(
        expr = reactive(quote(identity(data))),
        state = list()
      )
    }
  )
}
```

### Block `ui`

- The function signature is expected to contain `ns` in first place,
  followed by an arbitrary number of arguments that are aligned with
  `state` values such that this function can be called with an updated
  state.
- A call to appropriate shiny UI functions is expected that return
  `shiny.tag` or `shiny.tag.list` objects.
- Namespaces can be constructed using the `ns` function which should be
  passed as first argument the `moduleServer()` ID and as second
  argument the input ID.
- The initial evaluation of the `ui` function will be performed in the
  context of the constructor scope (i.e. if a value of name `xyz` is
  bound the the constructor scope or a parent thereof, this value will
  be passed as `xyz` argument).

The `ui` component of an identity transform block is trivial:

``` r
function(ns) {
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
    function() {
      moduleServer(
        "my_expr",
        function(input, output, session) {
          list(
            expr = reactive(
              bquote(
                `::`(.(pkg), .(dat)),
                list(pkg = package, dat = input$dataset)
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
    function(ns, dataset) {
      selectInput(
        inputId = ns("my_expr", "dataset"),
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
names bount in the constructor scope (e.g. `dataset` and `choices`). The
`state` entry returned by the server module contains the static value
`package` and this is needed for ser/deser.

The block can then be manipulated via UI as

``` r
serve(new_dataset_block("iris"))
```

For more examples, refer to `blockr.dplyr::new_select_block()` or
`blockr.dplyr::new_join_block()`.

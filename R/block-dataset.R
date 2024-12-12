#' Data block constructor
#'
#' This block allows to selected data from a package,
#' the default being datasets.
#'
#' @param dataset Selected dataset
#' @param package Name of an R package containing datasets
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_dataset_block <- function(dataset = character(), package = "datasets",
                              ...) {

  is_dataset_eligible <- function(x, pkg) {
    inherits(do.call("::", list(pkg = pkg, name = x)), "data.frame")
  }

  list_datasets <- function(package) {
    datasets <- utils::data(package = package)
    datasets <- datasets$results[, "Item"]

    options <- gsub("\\s+\\(.+\\)$", "", datasets)

    options[lgl_ply(options, is_dataset_eligible, package)]
  }

  new_data_block(
    function() {
      moduleServer(
        "expression",
        function(input, output, session) {
          list(
            expr = reactive(
              eval(
                bquote(
                  as.call(c(as.symbol("::"), quote(.(pkg)), quote(.(dat)))),
                  list(pkg = as.name(package), dat = as.name(input$dataset))
                )
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
    function(ns) {
      selectInput(
        inputId = ns("expression", "dataset"),
        label = "Dataset",
        choices = list_datasets(package),
        selected = dataset
      )
    },
    class = "dataset_block",
    ...
  )
}

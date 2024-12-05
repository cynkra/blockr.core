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

  new_block(
    quote(`::`(as.name(.(package)), as.name(.(dataset)))),
    class = c("dataset_block", "data_block"),
    state = list(
      dataset = dataset,
      options = list_datasets(package),
      package = package
    ),
    ...
  )
}

#' @rdname block_server
#' @export
fields_server.dataset_block <- function(x, input = list(), ...) {
  moduleServer(
    block_uid(x, prefix = "fields"),
    function(input, output, session) {
      list(
        dataset = reactive(input$dataset)
      )
    }
  )
}

#' @rdname block_ui
#' @export
fields_ui.dataset_block <- function(x, ...) {
  selectInput(
    inputId = block_ns(x, "dataset", prefix = "fields"),
    label = "Dataset",
    choices = block_state(x, "options"),
    selected = block_state(x, "dataset")
  )
}

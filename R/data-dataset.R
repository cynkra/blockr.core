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
    inherits(do.call("::", list(pkg, x)), "data.frame")
  }

  list_datasets <- function(package) {
    datasets <- utils::data(package = package)
    datasets <- datasets$results[, "Item"]

    options <- gsub("\\s+\\(.+\\)$", "", datasets)

    options[lgl_ply(options, is_dataset_eligible, package)]
  }

  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {

          dat <- reactiveVal(dataset)

          obs <- observeEvent(input$dataset, dat(input$dataset))

          list(
            expr = reactive({
              eval(
                bquote(
                  as.call(c(as.symbol("::"), quote(.(pkg)), quote(.(dat)))),
                  list(pkg = as.name(package), dat = as.name(dat()))
                )
              )
            }),
            state = list(
              dataset = dat,
              package = package
            ),
            obs = list(obs)
          )
        }
      )
    },
    function(id) {
      selectInput(
        inputId = NS(id, "dataset"),
        label = "Dataset",
        choices = list_datasets(package),
        selected = dataset
      )
    },
    class = "dataset_block",
    ...
  )
}

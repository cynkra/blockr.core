#' Subset block constructor
#'
#' This block allows to perform row and column subsetting on `data.frame`
#' objects (see [base::subset()]).
#'
#' @param n Number of rows
#' @param direction Either "head" or "tail"
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_head_block <- function(n = 6L, direction = c("head", "tail"), ...) {

  direction <- match.arg(direction)

  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          nrw <- reactiveVal(n)
          til <- reactiveVal(isTRUE(direction == "tail"))

          observeEvent(input$n, nrw(input$n))
          observeEvent(input$tail, til(input$tail))

          observeEvent(
            nrow(data()),
            updateNumericInput(
              inputId = "n",
              value = nrw(),
              min = 1L,
              max = nrow(data())
            )
          )

          list(
            expr = reactive(
              if (isTRUE(til())) {
                bquote(utils::tail(data, n = .(n)), list(n = nrw()))
              } else {
                bquote(utils::head(data, n = .(n)), list(n = nrw()))
              }
            ),
            state = list(
              n = nrw,
              direction = reactive(if (isTRUE(til())) "tail" else "head")
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
        ),
        bslib::input_switch(
          id = NS(id, "tail"),
          label = "Tail",
          value = isTRUE(direction == "tail")
        )
      )
    },
    dat_val = function(data) {
      stopifnot(is.data.frame(data))
    },
    class = "head_block",
    ...
  )
}

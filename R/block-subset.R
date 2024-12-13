#' Subset block constructor
#'
#' This block allows to perform row and column subsetting on `data.frame`
#' objects (see [base::subset()]).
#'
#' @param subset,select Expressions (passed as strings)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_subset_block <- function(subset = "", select = "", ...) {
  new_transform_block(
    function(data) {
      moduleServer(
        "expression",
        function(input, output, session) {
          list(
            expr = reactive({
              if (nzchar(input$subset) && nzchar(input$select)) {
                bquote(
                  subset(data, .(sub), .(sel)),
                  list(
                    sub = parse(text = input$subset)[[1L]],
                    sel = parse(text = input$select)[[1L]]
                  )
                )
              } else if (nzchar(input$select)) {
                bquote(
                  subset(data, select = .(sel)),
                  list(sel = parse(text = input$select)[[1L]])
                )
              } else if (nzchar(input$subset)) {
                bquote(
                  subset(data, .(sub)),
                  list(sub = parse(text = input$subset)[[1L]])
                )
              } else {
                quote(subset(data))
              }
            }),
            state = list(
              subset = reactive(input$subset),
              select = reactive(input$select)
            )
          )
        }
      )
    },
    function(ns, subset, select) {
      tagList(
        textInput(
          inputId = ns("expression", "subset"),
          label = "Subset",
          value = subset
        ),
        textInput(
          inputId = ns("expression", "select"),
          label = "Select",
          value = select
        )
      )
    },
    class = "subset_block",
    ...
  )
}

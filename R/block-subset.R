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
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive({
              subset_expr(input$subset, input$select)
            }),
            state = list(
              subset = reactive(input$subset),
              select = reactive(input$select)
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        textInput(
          inputId = NS(id, "subset"),
          label = "Subset",
          value = subset
        ),
        textInput(
          inputId = NS(id, "select"),
          label = "Select",
          value = select
        )
      )
    },
    dat_val = function(data) {
      stopifnot(is.data.frame(data))
    },
    class = "subset_block",
    ...
  )
}

subset_expr <- function(sub, sel) {

  pasrse_first <- function(x) {
    parse(text = x)[[1L]]
  }

  if (nzchar(sub) && nzchar(sel)) {
    bquote(
      subset(data, .(sub), .(sel)),
      list(sub = pasrse_first(sub), sel = pasrse_first(sel))
    )
  } else if (nzchar(sel)) {
    bquote(
      subset(data, select = .(sel)),
      list(sel = pasrse_first(sel))
    )
  } else if (nzchar(sub)) {
    bquote(
      subset(data, .(sub)),
      list(sub = pasrse_first(sub))
    )
  } else {
    quote(subset(data))
  }
}

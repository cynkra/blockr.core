#' Subset block constructor
#'
#' This block allows to perform row and column subsetting on `data.frame`
#' objects (see [base::subset()]).
#'
#' @param by Column(s) tp join on
#' @param all_x,all_y Join type, see [base::merge()]
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_merge_block <- function(by = character(), all_x = FALSE, all_y = FALSE,
                            ...) {

  by_choices <- function(x, y) {
    intersect(colnames(x), colnames(y))
  }

  new_transform_block(
    function(id, x, y) {
      moduleServer(
        id,
        function(input, output, session) {

          sels <- reactiveVal(by)

          allx <- reactiveVal(all_x)
          ally <- reactiveVal(all_y)

          observeEvent(input$by, sels(input$by))

          observeEvent(
            input$type,
            {
              allx("all.x" %in% input$type)
              ally("all.y" %in% input$type)
            },
            ignoreNULL = FALSE,
            ignoreInit = TRUE
          )

          cols <- reactive(by_choices(x(), y()))

          observe(
            updateSelectInput(
              session,
              inputId = "by",
              choices = cols(),
              selected = sels()
            )
          )

          observe(
            {
              updateCheckboxGroupInput(
                session,
                inputId = "type",
                choices = c("all.x", "all.y"),
                selected = c("all.x", "all.y")[c(allx(), ally())]
              )
            }
          )

          list(
            expr = reactive(
              bquote(
                merge(x, y, by = .(cols), all.x = .(allx), all.y = .(ally)),
                list(cols = sels(), allx = allx(), ally = ally())
              )
            ),
            state = list(
              by = sels,
              all_x = allx,
              all_y = ally
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        selectInput(
          inputId = NS(id, "by"),
          label = "By columns",
          choices = list(),
          multiple = TRUE
        ),
        checkboxGroupInput(
          inputId = NS(id, "type"),
          label = "Join type",
          choices = c("all.x", "all.y")
        )
      )
    },
    dat_valid = function(x, y) {
      stopifnot(is.data.frame(x), is.data.frame(y))
    },
    class = "merge_block",
    ...
  )
}

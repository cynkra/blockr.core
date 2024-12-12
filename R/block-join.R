#' Select block constructor
#'
#' This block allows for joining of two `data.frame` objects (see
#' [dplyr::left_join()]).
#'
#' @param type Join type
#' @param by Column(s) tp join on
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_join_block <- function(type = character(), by = character(), ...) {

  by_choices <- function(x, y) {
    intersect(colnames(x), colnames(y))
  }

  join_types <- paste0(
    c("left", "inner", "right", "full", "semi", "anti"),
    "_join"
  )

  if (length(type)) {
    type <- match.arg(type, join_types)
  } else {
    type <- join_types[1L]
  }

  by_opts <- by

  new_transform_block(
    function(x, y) {
      moduleServer(
        "expression",
        function(input, output, session) {

          sels <- reactiveVal(by)
          cols <- reactive(by_choices(x(), y()))

          observeEvent(input$by, sels(input$by))

          observe(
            updateSelectInput(
              session,
              inputId = "by",
              choices = cols(),
              selected = sels()
            )
          )

          list(
            expr = reactive(
              bquote(
                .(func)(x, y, by = .(cols)),
                list(
                  func = eval(
                    bquote(
                      as.call(c(as.symbol("::"), quote(dplyr), quote(.(fun)))),
                      list(fun = as.name(input$type))
                    )
                  ),
                  cols = sels()
                )
              )
            ),
            state = list(
              type = reactive(input$type),
              by = reactive(sels()),
              by_opts = reactive(cols())
            )
          )
        }
      )
    },
    function(ns) {
      tagList(
        selectInput(
          inputId = ns("expression", "type"),
          label = "Join type",
          choices = join_types,
          selected = type
        ),
        selectInput(
          inputId = ns("expression", "by"),
          label = "By columns",
          choices = by_opts,
          selected = by,
          multiple = TRUE
        )
      )
    },
    class = "join_block",
    ...
  )
}

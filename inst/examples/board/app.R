library(scoutbaR)
library(shiny)
library(bslib)
library(visNetwork)

devtools::load_all()

ui <- page_fillable(
  board_ui("board")
)

server <- function(input, output, session) {
  board_server("board")
}

shinyApp(ui, server)
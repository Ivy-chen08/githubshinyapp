library(shiny)

ui <- navbarPage(
  title = "DATA2x02 Survey Explorer",
  tabPanel("Home", h2("Welcome"), p("Placeholder")),
  tabPanel("Data Visualization", h2("Coming soon")),
  tabPanel("Statistical Tests", h2("Coming soon"))
)

server <- function(input, output, session) {}

shinyApp(ui, server)
library(shiny)

# Point d'entrée explicite (évite les ambiguïtés ui.R/server.R)
ui <- source("ui.R", local = TRUE)$value
server <- source("server.R", local = TRUE)$value

shinyApp(ui = ui, server = server)


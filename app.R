library(shiny)



source("UI.R")
source("server.R")


# Run the Shiny app
shinyApp(ui = ui, server = server)
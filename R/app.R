in#load libraries
library(shiny)

#load ui and server
source("ui.R")
source("server.R")

shinyApp(ui, server)


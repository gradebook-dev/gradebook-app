#load libraries
library(shinydashboard)
library(shiny)
#library(shinyWidgets)
library(DT)
library(tidyverse)
#library(gradebook)
library(yaml)

#load ui and server
source("ui.R")
source("server.R")

shinyApp(ui, server)
#load libraries
library(shinydashboard)
library(shiny)
library(DT)
library(shinyWidgets)
library(tidyverse)
library(gradebook)
library(yaml)

#load ui and server
source("ui.R")
source("server.R")

shinyApp(ui, server)
# load libraries
library(shinyWidgets)
library(DT)
library(tidyverse)
library(gradebook)

#load helper scripts
HSLocation <- "helperscripts/"
source(paste0(HSLocation, "categories.R"), local = TRUE)
source(paste0(HSLocation, "assignments.R"), local = TRUE)

shinyServer(function(input, output, session) {

#### -------------------------- UPLOADS ----------------------------####   
    
    #testing
    data <- reactive({
        req(input$upload)
        
        tryCatch({
            data <- gradebook::read_gs(input$upload$datapath)
            return(data)
        }, error = function(e) {
            validate("Invalid file; Please upload a .csv")
            return(NULL)
        })
    })
    
#### -------------------------- ASSIGNMENTS ----------------------------#### 

    #keeps track of which category each assignment is assigned to
    assign <- reactiveValues(table = NULL)
    
    # creates assigns table when data uploads 
    # all assignments default to "Unassigned"
    observe({
        colnames <- get_assignments(data())
        assign$table <- data.frame(colnames, category = c("Unassigned"))
    })
    
    #a list of unassigned assignments in policies tab
    output$unassigned <- renderUI(
        if (!is.null(assign$table)){
            HTML(markdown::renderMarkdown(text = paste(paste0("- ", getUnassigned(assign$table), "\n"), collapse = "")))
        } else {
            textOutput("unassigned_message")
        }
    )
    output$unassigned_message <- renderText({"Let's upload some data first..."})
    
    
#### -------------------------- SCRATCHPAD ----------------------------####   
    output$original_gs <- renderDataTable({
        datatable(data(), options = list(scrollX = TRUE, scrollY = "500px"))
    })
    
    output$assigns_table <- renderDataTable({ assign$table })
    
})

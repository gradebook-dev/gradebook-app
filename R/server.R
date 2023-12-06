# load libraries
library(shinyWidgets)
library(DT)
library(tidyverse)
library(gradebook)

#load helper scripts
HSLocation <- "helperscripts/"
source(paste0(HSLocation, "categories.R"), local = TRUE)
source(paste0(HSLocation, "assignments.R"), local = TRUE)
source(paste0(HSLocation, "grades.R"), local = TRUE)

shinyServer(function(input, output, session) {

#### -------------------------- UPLOAD A FILE ----------------------------####   
    
    #testing
    data <- reactive({
        req(input$upload)
        
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = gradebook::read_gs(input$upload$datapath),
               validate("Invalid file; Please upload a .csv")
        )
    })
    
    
    
#### -------------------------- SCRATCHPAD ----------------------------####   
    output$original_gs <- renderDataTable({
        datatable(
            data(),
            options = list(scrollX = TRUE)
        )
        
    })
    
})

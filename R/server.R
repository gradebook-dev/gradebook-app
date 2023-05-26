# load libraries
library(shinyWidgets)

#load helper scripts
HSLocation <- "helperscripts/"
source(paste0(HSLocation, "categories.R"), local = TRUE)


shinyServer(function(input, output, session) {
    
    observeEvent(input$new_cat, {
        showModal(edit_category)
    })
    
})

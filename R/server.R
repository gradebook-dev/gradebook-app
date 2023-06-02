# load libraries
library(shinyWidgets)

#load helper scripts
HSLocation <- "helperscripts/"
source(paste0(HSLocation, "categories.R"), local = TRUE)


shinyServer(function(input, output, session) {

### -------------------------- UPLOAD A FILE ----------------------------###   
    
    data <- reactive({
        req(input$upload)
        
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ",", na = c("", "NA")),
               validate("Invalid file; Please upload a .csv or .tsv file")
        )
    })
    
    #shows inputted .csv in Scratchpad
    output$input_data <- renderDataTable({
        data <- data()
        if(is.null(input$upload)){
            return("Upload some data first")
        }
        else{
            read.table(input$upload$datapath, sep = ",", header = TRUE, fill=TRUE)
            
        }
    })
    
### -------------------------- NEW CATEGORY MODAL ----------------------------###
    observeEvent(input$edit_cat, {
        showModal(edit_category_modal)
    })
    
    observeEvent(input$new_cat, {
        showModal(new_category_modal)
    })
    
    observeEvent(input$cancel, {
        removeModal() 
    })
    
    
### -------------------------- POLICY-COURSE NAME  ----------------------------###
    # initialize class_name and class_description as reactive values
    policy <- reactiveValues(coursewide = list(course_name = "Your Course Name",
                                              description = "This is the description of what the policy file is for and author/date info. 
                                              Any course-wide policies could go here (total slip days, total drops, letter grade cutoffs)"),
                             categories = list())
    
    observeEvent(input$edit_policy_name, {
        showModal(modalDialog(
            title = "Edit Policy",
            textInput("course_name_input", "Course Name", value = policy$coursewide$course_name),
            textInput("course_desc_input", "Course Description", value = policy$coursewide$description),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("save_changes_course", "Save Changes")
            )
        ))
    })
    
    # When save_changes is clicked, update the reactive values and close modal
    observeEvent(input$save_changes_course, {
        policy$coursewide$course_name <- isolate(input$course_name_input)
        policy$coursewide$description <- isolate(input$course_desc_input)
        removeModal()
    })
    # Update course_name in policy tab
    output$course_name_display <- renderText({
        policy$coursewide$course_name
    })
    # Update course description in policy tab
    output$course_description_display <- renderText({
        policy$coursewide$description
    })
 

    
### -------------------------- NEXT FUNCTIONALITY...  ----------------------------###  

    
    
})

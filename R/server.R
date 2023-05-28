# load libraries
library(shinyWidgets)
library(DT)

#load helper scripts
HSLocation <- "helperscripts/"
source(paste0(HSLocation, "categories.R"), local = TRUE)


shinyServer(function(input, output, session) {

#### -------------------------- UPLOAD A FILE ----------------------------#### 
    
    data <- reactive({
        req(input$upload)
        
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ",", na = c("", "NA")),
               validate("Invalid file; Please upload a .csv or .tsv file")
        )
    })
    
    output$input_data <- renderDataTable({
        data <- data()
        if(is.null(input$upload)){
            return("Upload some data first")
        }
        else{
            read.table(input$upload$datapath, sep = ",", header = TRUE, fill=TRUE)
        }
    })
    
#### -------------------------- NEW CATEGORY MODAL ----------------------------####
    # observeEvent(input$edit_cat, {
    #     showModal(edit_category_modal)
    # })
    #Note: addCategory and deleteCategory functions are in categories.R
    editing <- reactiveValues(name = NULL, #editing$name saves the original name for the category we're updating
                             new = NULL) #remembers if this category is new with default values
    
    observeEvent(input$new_cat, {
        cat$list <- addCategory(cat$list) #adds new category with default values
        showModal(edit_category_modal)
        i <- length(cat$list$name)
        updateModalValues(paste0("New Category ", i)) #updates all UI in modal, function defined below
        editing$name <- paste0("New Category ", i)
        editing$new <- TRUE
        
    })
    
    observeEvent(input$cancel, {
        if (editing$new){
            i <- length(cat$list$name)
            cat$list <- deleteCategory(cat$list, editing$name) #deletes default category   
        }
        editing$new <- NULL
        removeModal() 
    })
    
    observeEvent(input$save, {
        cat$list <- updateCategory(cat$list, input, editing$name)
        removeModal() 
        editing$name <- NULL
        editing$new <- NULL
    })
    
 #### -------------------------- UPDATED EDIT MODAL  ----------------------------####    
updateModalValues <- function(cat_name){
    #updated edit_category_modal with info from category "cat_name"
    i <- which(cat$list$name == cat_name)
    updateTextInput(session, "change_cat_name", value = cat$list$name[i])
    updateAutonumericInput(session, "slip", value = cat$list$slip_days[i])
    updateTextInput(session, "late_allowed1", value = cat$list$late_time1[i])
    updateTextInput(session, "late_allowed2", value = cat$list$late_time2[i])
    updateAutonumericInput(session, "late_penalty1", "", value = cat$list$late_scale1[i])
    updateAutonumericInput(session, "late_penalty2", "", value = cat$list$late_scale2[i])
    updateAutonumericInput(session, "weight", "", value = cat$list$weight[i])
    updateAutonumericInput(session, "num_drops", "", value = cat$list$drops[i])
    updateSelectInput(session, "grading_policy", selected = cat$list$aggregation[i])
    updateSelectInput(session, "clobber_with", selected = cat$list$clobber[i])
    updateSelectizeInput(session, "assign", selected = cat$list$assigns[i])
}
    
#### -------------------------- POLICY-COURSE NAME  ----------------------------####
    # initialize class_name and class_description as reactive values
    course_name_rv <- reactiveValues(course_name = "Your Course Name", 
                                     course_description = "This is the description of what the policy file is for and author/date info. Any course-wide policies could go here (total slip days, total drops, letter grade cutoffs)")
    
    observeEvent(input$edit_policy_name, {
        showModal(modalDialog(
            title = "Edit Policy",
            textInput("course_name_input", "Course Name", value = course_name_rv$course_name),
            textInput("course_desc_input", "Course Description", value = course_name_rv$course_description),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("save_changes_course", "Save Changes")
            )
        ))
    })
    
    # When save_changes is clicked, update the reactive values and close modal
    observeEvent(input$save_changes_course, {
        course_name_rv$course_name <- isolate(input$course_name_input)
        course_name_rv$course_description <- isolate(input$course_desc_input)
        removeModal()
    })
    # Update course_name in policy tab
    output$course_name_display <- renderText({
        course_name_rv$course_name
    })
    # Update course description in policy tab
    output$course_description_display <- renderText({
        course_name_rv$course_description
    })
 

    
#### -------------------------- CAT$LIST  ----------------------------####  
    #create reactive list for all category criteria
    cat <- reactiveValues(list = NULL) 
    
    #outputs cat_list in Scratchpad
    output$cat_list <- renderTable(as.data.frame(cat$list))
    
    
})

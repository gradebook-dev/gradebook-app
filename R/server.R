# load libraries
library(shinyWidgets)
library(tidyverse)
library(DT)

#load helper scripts
HSLocation <- "helperscripts/"
source(paste0(HSLocation, "categories.R"), local = TRUE)
source(paste0(HSLocation, "Assignments.R"), local = TRUE)
source(paste0(HSLocation, "ProcessSID.R"), local = TRUE)

shinyServer(function(input, output, session) {

#### -------------------------- UPLOAD A FILE ----------------------------#### 
    #saves uploaded .csv in data
    data <- reactive({
        req(input$upload)
        
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ",",
                                  na = c("", "NA")),
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
    

#### -------------------------- ASSIGNMENTS ----------------------------####

    #reactive unassigned assignments table
    assign <- reactiveValues(table = NULL)
    
    #takes reactive data output and creates a reactive assignment table
    #contains all the columns from the original dataframe(names, emails, all columns from assignments, etc)
    assignments <- reactive({
        data <- data()
        createAssignTable(data)
    })
    
    #creates unassigned assignments table, excludes all names, sections, latenes, etc...
    observe({
        data <- data()
        assign$table <- createAssignTable(data)%>%
            filter(!str_detect(colnames, "Name|Sections|Max|Time|Late|Email|SID"))
    })
    
    output$assign <- renderDataTable({
        assign$table
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

#### -------------------------- PIVOT + STUDENT IDS ----------------------------####
    new_data <- reactive({
        # Get the new column names from the form data frame
        new_colnames <- assignments()$new_colnames
        # Rename the columns of the data frame using the new column names
        data_new_colnames <- data() %>%
            rename(!!!setNames(names(.), new_colnames))
        #fix dates
        new_time <- data_new_colnames%>%
            mutate(across(contains("submission_time"), lubridate::mdy_hm), #convert to datetimes , previous format: lubridate::ymd_hms
                   across(contains("lateness"), convert_to_min),
                   across(contains("lateness"), as.character))
        return(new_time)
    })
    # this allows lubridate values to be saved in the dataframe
    convert_to_min <- function(hms){
        to_hms <- lubridate::hms(hms)
        to_seconds <- period_to_seconds(to_hms)
        to_minutes <- to_seconds/60
        return (to_minutes)
    }
    
    output$new_data <- renderDataTable({
        new_data()
    })
    
    processed_sids <- reactive({
        new_data <- new_data()
        process_sids(new_data)
    })

    pivotdf <- reactive({
        processed_sids <- processed_sids()$unique_sids

        pivot(processed_sids, assign$table, cat$list)
    })

    output$pivotdf <- renderDataTable({
        pivotdf()
    })
#### -------------------------- NEW CATEGORY MODAL ----------------------------####

    #Note: addCategory and deleteCategory functions are in categories.R
    editing <- reactiveValues(name = NULL, #editing$name saves the original name for the category we're updating
                             new = NULL,   #remembers if this category is new with default values
                             num = 1)      #used to make unique ids for category cards
    #when you create a new category
    observeEvent(input$new_cat, {
        cat$list <- addCategory(cat$list, editing$num) #adds new category with default values
        editing$num <- editing$num + 1
        showModal(edit_category_modal) #opens edit modal
        i <- length(cat$list$name)
        updateModalValues(paste0("New Category ", i)) #updates all UI in modal, function defined below
        editing$name <- paste0("New Category ", i) #saves original name of category
        editing$new <- TRUE #this is a new category with default value
        
    })
    
    observeEvent(input$cancel, {
        if (editing$new){
            i <- length(cat$list$name)
            cat$list <- deleteCategory(cat$list, editing$name) #deletes default category   
        }
        editing$name <- NULL #reset all editing values
        editing$new <- NULL
        removeModal() 
    })
    
    
#### -------------------------- CATEGORY CARDS  ----------------------------#### 
    observeEvent(input$save, {
        cat$list <- updateCategory(cat$list, input, editing$name)
        if (!is.null(assign$table)){
            assign$table <- resetAssigns(assign$table, editing$name)
            assign$table <- updateAssigns(assign$table, input$assign, input$change_cat_name)   
        }
        if (editing$new){ #if we're making a new category
            i <- length(cat$list$name)
            nr <- cat$list$nr[i]
            insertUI( #creates UI for this category
                selector = '#inputList',
                ui=div(
                    id = paste0("cat",nr),
                    div(
                        style = "border: 1px solid #000; padding: 10px; border-radius: 5px; margin-top: 20px;",
                        tags$div(
                        style = "display: flex; justify-content: left; align-items: center;",
                        
                        tags$div(
                        h4(cat$list$name[i]),
                        style = "margin-right: 10px;"),
                        #rest of information about this category will be here
                        actionButton(paste0('delete',nr), label = NULL, icon = icon("trash-can"),  style = "background-color: transparent; margin-right: 10px;"), #remove button for this category
                        #edit button
                        actionButton(paste0('edit',nr), label = NULL, icon = icon("pen-to-square"), style = "background-color: transparent; ")
                        ),
                        update_ui_categories(cat$list, nr)
                    
                        
                )
               
                )
            )
            
            observeEvent(input[[paste0('edit',nr)]],{
                showModal(edit_category_modal) #opens edit modal
                i <- which(cat$list$nr == nr)
                nr <- cat$list$nr[i]
                updateModalValues(cat$list$name[i]) #updates all UI in modal, function defined below
                editing$name <- cat$list$name[i] #saves original name of category
                editing$new <- FALSE #this is a new category with default value
                update_ui_categories(cat$list, nr)
            })
            
            observeEvent(input[[paste0('delete',nr)]],{
                i <- which(cat$list$nr == nr)
                cat$list <- deleteCategory(cat$list, cat$list$name[i]) #if this remove button pressed, it deletes this category
                removeUI(
                    selector = paste0("#cat",nr) #this removes the UI for this category
                )
            })
        }
        removeModal() 
        editing$name <- NULL  #reset all editing values
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
    
    choices <- ""
    if (!is.null(assign$table)){
        choices <- assign$table %>% filter (category == "Unassigned") %>% select(colnames) 
    }
    # Preload selected values
    preloaded_values <- cat$list$assigns[i]
    if (length(preloaded_values) != 0){
        preloaded_values <- unlist(strsplit(preloaded_values, ", ")) # Split the string and unlist the result
        choices = c(choices, preloaded_values)
    }
    updateSelectizeInput(session, "assign", selected = strsplit(cat$list$assigns[i], ", ")[[1]])
    updateSelectizeInput(session, "assign", choices = choices, selected = preloaded_values)
}
    
    
#### -------------------------- CAT$LIST  ----------------------------####  
    #create reactive list for all category criteria
    cat <- reactiveValues(list = NULL) 
    
    #outputs cat_list in Scratchpad
    output$cat_list <- renderTable(as.data.frame(cat$list))
    
    
})

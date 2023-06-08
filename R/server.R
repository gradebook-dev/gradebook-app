# load libraries
library(shinyWidgets)
library(DT)
library(tidyverse)

#load helper scripts
HSLocation <- "helperscripts/"
source(paste0(HSLocation, "categories.R"), local = TRUE)
source(paste0(HSLocation, "assignments.R"), local = TRUE)
source(paste0(HSLocation, "process-sid.R"), local = TRUE)

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
    
    #shows inputted .csv in Scratchpad
    output$input_data <- renderDataTable({
        data <- data()
        if(is.null(input$upload)){
            return("Upload some data first")
        }
        else{
            datatable(
                read.table(input$upload$datapath, sep = ",", header = TRUE, fill=TRUE),
                options = list(scrollX = TRUE)
            )
            
        }
    })
    
#### -------------------------- POLICY-COURSE NAME  ----------------------------####
    # initialize class_name and class_description as reactive values
    policy <- reactiveValues(coursewide = list(course_name = "Your Course Name",
                                               description = "This is the description of what the policy file is for and author/date info. 
                                              Any course-wide policies could go here (total slip days, total drops, letter grade cutoffs)"),
                             categories = list())
    
    #shows policy$categories in Scratchpad under cat_list tab
    output$cat_list <- renderPrint({
        Hmisc::list.tree(policy$categories)
        })
    
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
#### -------------------------- NEW CATEGORY MODAL ----------------------------####
    
    #Note: addCategory and deleteCategory functions are in categories.R
    editing <- reactiveValues(num = 1,    #used to make unique ids for category card
                              nr = NULL) #used to differentiate editing with making new category
     
    
    observeEvent(input$new_cat, {
        editing$nr <- paste0("cat", editing$num)
        showModal(edit_category_modal) #opens edit modal
        #updates values that aren't always the same but still default
        updateTextInput(session, "change_cat_name", value = paste0("Category ", editing$num))
        if (!is.null(assign$table)){ #updates assignments if data has been loaded
            choices <- assign$table %>% filter (category == "Unassigned") %>% select(colnames)
            updateSelectizeInput(session, "assign", choices = choices, selected = "")
            updateSelectInput(session, "clobber_with", choices = c("None", select(assign$table, colnames)))
        }
        
    })
    
    observeEvent(input$cancel, {
        removeModal()
    })
    
    updateModalValues <- function(edit_nr){
        #updated edit_category_modal with info from category with nr edit_num
        i <- getCatIndex(policy$categories, edit_nr)
        updateTextInput(session, "change_cat_name", value = policy$categories[[i]]$name)
        updateAutonumericInput(session, "slip", value = policy$categories[[i]]$slip_days)
        updateTextInput(session, "late_allowed1", value = policy$categories[[i]]$late_time1)
        updateTextInput(session, "late_allowed2", value = policy$categories[[i]]$late_time2)
        updateAutonumericInput(session, "late_penalty1", "", value = policy$categories[[i]]$late_scale1)
        updateAutonumericInput(session, "late_penalty2", "", value = policy$categories[[i]]$late_scale2)
        updateAutonumericInput(session, "weight", "", value = policy$categories[[i]]$weight)
        updateAutonumericInput(session, "num_drops", "", value = policy$categories[[i]]$drops)
        updateSelectInput(session, "grading_policy", selected = policy$categories[[i]]$aggregation)
        updateSelectInput(session, "clobber_with", selected = policy$categories[[i]]$clobber)
        choices <- ""
        if (!is.null(assign$table)){
            choices <- assign$table %>% filter (category == "Unassigned") %>% select(colnames)
            updateSelectInput(session, "clobber_with", choices = c("None", select(assign$table, colnames)))
        }
        # Preload selected values
        preloaded_values <- policy$categories[[i]]$assigns
        if ((preloaded_values) != "None"){
            preloaded_values <- unlist(strsplit(preloaded_values, ", ")) # Split the string and unlist the result
            choices = c(choices, preloaded_values)
        }
        updateSelectizeInput(session, "assign", selected = strsplit(policy$categories[[i]]$assigns, ", ")[[1]])
        updateSelectizeInput(session, "assign", choices = choices, selected = preloaded_values)
    }
    
#### -------------------------- CATEGORY CARDS  ----------------------------#### 
    
    observeEvent(input$save, {
        i <- getCatIndex(policy$categories, editing$nr)
        original_name <- input$change_cat_name #if this is a new cateogyr 
        if (i < length(policy$categories)){ #if it's not a new category
            original_name <- policy$categories[[i]]$name 
        }
        policy$categories <- updateCategory(policy$categories, input, editing$nr)
        assign$table <- updateAssigns(assign$table, input$assign, original_name, input$change_cat_name)
        removeModal()
        editing$num <- editing$num + 1
        
        
        
        #UI below
        for (i in 1:length(policy$categories)){ #iterates through all categories
            nr <- policy$categories[[i]]$nr
            removeUI(
                selector = paste0("#cat",nr) #this removes the UI for this category
            )
            insertUI( #creates UI for this category
                selector = '#inputList',
                ui=div(
                    id = paste0("cat",nr),
                    div(
                        style = "border: 1px solid #000; padding: 10px; border-radius: 5px; margin-top: 20px;",
                        tags$div(
                            style = "display: flex; justify-content: left; align-items: center;",
                            
                            tags$div(
                                h4(policy$categories[[i]]$name),
                                style = "margin-right: 10px;"),
                            #rest of information about this category will be here
                            actionButton(paste0('delete',nr), label = NULL, icon = icon("trash-can"),  style = "background-color: transparent; margin-right: 10px;"), #remove button for this category
                            #edit button
                            actionButton(paste0('edit',nr), label = NULL, icon = icon("pen-to-square"), style = "background-color: transparent; ")
                        ),
                        update_ui_categories(policy$categories, nr)
                        
                        
                    )
                    
                )
            )
            
            
            observeEvent(input[[paste0('edit',nr)]],{
                showModal(edit_category_modal) #opens edit modal
                editing$nr <- nr
                updateModalValues(nr)
            })
            
            observeEvent(input[[paste0('delete',nr)]],{
                i <- getCatIndex(policy$categories, nr)
                assign$table <- resetAssigns(assign$table, policy$categories[[i]]$name)
                policy$categories <- deleteCategory(policy$categories, nr) #if this remove button pressed, it deletes this category
                removeUI(
                    selector = paste0("#cat",nr) #this removes the UI for this category
                )
            })
        }
    })
    
    
#### -------------------------- POLICY-COURSE NAME  ----------------------------####
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
    
#### -------------------------- ASSIGNMENTS  ----------------------------####  
    #reactive unassigned assignments table
    assign <- reactiveValues(table = NULL)
    
    observe({
        # if (length(policy$categories) != 0){
        #     for (i in 1:length(policy$categories)){
        #         name <- policy$categories[[i]]$name
        #         assign <- policy$categories[[i]]$assigns
        #         assigns$table <- updateCategory(assigns$table, assign, name)
        #     }   
        # }
    })
    
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
        save <- lubridate::hms(hms)
        save <- period_to_seconds(save)
        save <- save/60
        return (save)
    }
    
    output$new_data <- renderDataTable({
        datatable(new_data(),
        options = list(scrollX = TRUE))
    })
    
    processed_sids <- reactive({
        new_data <- new_data()
        process_sids(new_data)
    })
    
    pivotdf <- reactive({
        processed_sids <- processed_sids()$unique_sids
        
        pivot(processed_sids, assign$table, policy$categories)
    })
    
    output$pivotdf <- renderDataTable({
        datatable(
            pivotdf(),
            options = list(scrollX = TRUE)
        )
        })
    
    #### -------------------------- JSON ----------------------------####
    path <- "../../gradebook-data"
    dir.create(path, showWarnings = FALSE)
    
    #save config
    observeEvent(input$save_json, {
        p_list1 <- policy$coursewide
        p_list2 <- policy$categories
        jsonlite::write_json(list(p_list1, p_list2), paste(path, "cat_table.json", sep = "/"))
    })
    
    #load config
    observeEvent(input$upload_json, {
        if (file.exists(paste(path, "cat_table.json", sep = "/"))) {
            df <- jsonlite::fromJSON(paste(path, "cat_table.json", sep = "/"))
            policy$coursewide <- df[[1]]
            policy$categories <- df[[2]]
        } else {
            print("File not found")
        }
    })
    
})

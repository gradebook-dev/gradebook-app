# load libraries
library(shinyWidgets)
library(DT)

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
            read.table(input$upload$datapath, sep = ",", header = TRUE, fill=TRUE)
            
        }
    })
    
#### -------------------------- POLICY-COURSE NAME  ----------------------------####
    # initialize class_name and class_description as reactive values
    policy <- reactiveValues(coursewide = list(course_name = "Your Course Name",
                                               description = "This is the description of what the policy file is for and author/date info. 
                                              Any course-wide policies could go here (total slip days, total drops, letter grade cutoffs)"),
                             categories = list())
    
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
    editing <- reactiveValues(num = 1)      #used to make unique ids for category cards
     
    
    observeEvent(input$new_cat, {
        showModal(edit_category_modal) #opens edit modal
        policy$categories <- addCategory(policy$categories, editing$num) #adds new category with default values
        updateModalValues(editing$num) #updates all UI in modal, function defined below
    })
    
    observeEvent(input$cancel, {
        policy$categories <- deleteCategory(policy$categories, editing$num) #delete default category
        removeModal() 
    })
    
    observeEvent(input$save, {
        editing$num <- editing$num + 1 #increases with the making of a new category
        removeModal() 
    })
    
    updateModalValues <- function(edit_num){
        #updated edit_category_modal with info from category with nr edit_num
        i <- getCatIndex(policy$categories, editing$num)
         updateTextInput(session, "change_cat_name", value = policy$categories[[i]]$name[i])
         updateAutonumericInput(session, "slip", value = policy$categories[[i]]$slip_days[i])
         updateTextInput(session, "late_allowed1", value = policy$categories[[i]]$late_time1[i])
        updateTextInput(session, "late_allowed2", value = policy$categories[[i]]$late_time2[i])
        updateAutonumericInput(session, "late_penalty1", "", value = policy$categories[[i]]$late_scale1[i])
        updateAutonumericInput(session, "late_penalty2", "", value = policy$categories[[i]]$late_scale2[i])
        updateAutonumericInput(session, "weight", "", value = policy$categories[[i]]$weight[i])
        updateAutonumericInput(session, "num_drops", "", value = policy$categories[[i]]$drops[i])
        updateSelectInput(session, "grading_policy", selected = policy$categories[[i]]$aggregation[i])
        updateSelectInput(session, "clobber_with", selected = policy$categories[[i]]$clobber[i])
        
        choices <- ""
        if (!is.null(assign$table)){
            choices <- assign$table %>% filter (category == "Unassigned") %>% select(colnames)
        }
        # Preload selected values
        preloaded_values <- policy$categories[[i]]$assigns[i]
        if (length(preloaded_values) != 0){
            preloaded_values <- unlist(strsplit(preloaded_values, ", ")) # Split the string and unlist the result
            choices = c(choices, preloaded_values)
        }
        updateSelectizeInput(session, "assign", selected = strsplit(policy$categories[[i]]$assigns[i], ", ")[[1]])
        updateSelectizeInput(session, "assign", choices = choices, selected = preloaded_values)
    }
    
#### -------------------------- CATEGORY CARDS  ----------------------------#### 
    
    
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
})

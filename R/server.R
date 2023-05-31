# load libraries
library(shinyWidgets)
library(tidyverse)
library(DT)

#load helper scripts
HSLocation <- "helperscripts/"
source(paste0(HSLocation, "categories.R"), local = TRUE)
source(paste0(HSLocation, "Assignments.R"), local = TRUE)

shinyServer(function(input, output, session) {

#### -------------------------- UPLOAD A FILE ----------------------------#### 
    #saves uploaded .csv in data
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

#### -------------------------- ASSIGNMENTS ----------------------------####

    assign <- reactiveValues(table = NULL)
    
    observe({
        data <- data()
        assign$table <- createAssignTable(data)%>%
            filter(!str_detect(colnames, "Name|Sections|Max|Time|Late|Email|SID"))
    })
    
    output$assign <- renderDataTable({
        assign$table
        })
    
    output$unassigned <- renderUI(
        if (!is.null(assign$table)){
            HTML(markdown::renderMarkdown(text = paste(paste0("- ", getUnassigned(assign$table), "\n"), collapse = "")))
        } else {
            textOutput("unassigned_message")
        }
    )
    
    output$unassigned_message <- renderText({"Let's upload some data first..."})

#### -------------------------- PIVOT + STUDENT IDS ----------------------------####
        
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
    updateSelectizeInput(session, "assign", selected = strsplit(cat$list$assigns[i], ", ")[[1]])
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

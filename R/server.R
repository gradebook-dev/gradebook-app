# load libraries
library(shinyWidgets)
library(DT)
library(tidyverse)
library(gradebook)
library(yaml)

#load helper scripts
HSLocation <- "helperscripts/"
source(paste0(HSLocation, "categories.R"), local = TRUE)
source(paste0(HSLocation, "assignments.R"), local = TRUE)
source(paste0(HSLocation, "exceptions.R"), local = TRUE)

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
    
#### -------------------------- POLICY ----------------------------####  
    policy <- reactiveValues(coursewide = list(course_name = "Course Name", description = "Description"),
                             categories = list(),
                             letter_grades = list(),
                             exceptions = list(),
                             flat = list())
    
#### -------------------------- COURSEWIDE ----------------------------####
    output$course_name_display <- renderText({policy$coursewide$course_name})
    
    output$course_description_display <- renderText({policy$coursewide$description})
    
#### -------------------------- CATEGORY MODAL ----------------------------####
    editing <- reactiveValues(name = NULL) #to keep track of edited category
    
    observeEvent(input$new_cat, {
        editing$name <- NULL
        showModal(edit_category_modal) #opens edit modal
        #updates values that aren't always the same but still default
        updateTextInput(session, "name", value = paste0("Category ", editing$num))
        if (!is.null(assign$table)){ #updates assignments if data has been loaded
            updateSelectizeInput(session, "assignments", choices = assign$table$assignment, selected = "")
        }
        
    })
    
    output$lateness <- renderUI({
        if (input$num_lateness > 0){
            lapply(1:as.integer(input$num_lateness), function(i) {
                fluidRow(
                    column(4,
                           textInput(inputId = paste0("from", i), label = "From:", value = "",
                                     placeholder = "HH:MM:SS")
                    ),
                    column(4,
                           textInput(inputId = paste0("to", i), label = "To:", value = "",
                                     placeholder = "HH:MM:SS")
                    ),
                    column(4,
                           numericInput(inputId = paste0("scale", i), label = "Scale by:", value = "")
                    )
                )
            })
        }
    })
    
    observeEvent(input$cancel,{
        removeModal() #closes edit modal
    })
    
    observeEvent(input$save,{
        removeModal() #closes edit modal
        #update policy
        if (!is.null(editing$name)){
            policy$categories <- updateCategory(policy$categories, policy$flat, editing$name, 
                                                input$name, input, assign$table)
        } else {
            policy$categories <- append(policy$categories, 
                                        list(createCategory(input$name, input = input,
                                                            assign$table)))
        }

    })
    
    #whenever policy$categories changes, policy$flat, assign$table and UI updates
    observe({
        policy$flat <- list(categories = policy$categories) |> gradebook::flatten_policy()
        #assign$table <- updateAssignsTable(assign$table, input, policy$flat)
        names <- purrr::map(policy$flat$categories, "category") |> unlist()
        purrr::walk(names, rerender_ui)
    })
    
#### -------------------------- DISPLAY POLICY ----------------------------####

    
    rerender_ui <- function(name) { #render category UI for policy page
        i <- getIndex(policy$flat, name)
        label <- gsub(pattern = "[^a-zA-Z0-9]+", replacement = "", name)
        removeUI(
            selector = paste0("#cat",label) #this removes the UI for this category
        )
        insertUI( #creates UI for this category
            selector = '#inputList',
            ui=div(id = paste0("cat",label),
                   box(
                    title = policy$flat$categories[[i]]$category, status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    "Weight: ", policy$flat$categories[[i]]$weight, br(),
                    "Aggregation: ", policy$flat$categories[[i]]$aggregation, br(),
                    "Assignments: ",paste(policy$flat$categories[[i]]$assignments, collapse = ", "), br(),
                    actionButton(paste0('delete',label), label = NULL, icon = icon("trash-can"),  style = "background-color: transparent; margin-right: 10px;"), #remove button for this category
                    actionButton(paste0('edit',label), label = NULL, icon = icon("pen-to-square"), style = "background-color: transparent; "),
                    width = "100%"
                ))
        )
        
        observeEvent(input[[paste0('edit',label)]],{
            
            showModal(edit_category_modal) #opens edit modal
            editing$name <- label
            i <- getIndex(policy$flat, label)
            updateTextInput(session, "name", value = policy$flat$categories[[i]]$category)
            updateSelectInput(session, "aggregation", selected = policy$flat$categories[[i]]$aggregation)
            if (!is.null(policy$flat$categories[[i]]$weight)){
                shinyWidgets::updateAutonumericInput(session, "weight", value = policy$flat$categories[[i]]$weight)   
            }
            if (!is.null(policy$flat$categories[[i]]$n_drops)){
                updateNumericInput(session, "n_drops", value = policy$flat$categories[[i]]$n_drops*100)
            }
            if (!is.null(policy$flat$categories[[i]]$clobber)){
                updateSelectInput(session, "clobber", selected = policy$flat$categories[[i]]$clobber)
            }
            updateSelectizeInput(session, "assignments", selected = (policy$flat$categories[[i]]$assignments),
                                 choices = c(assign$table$assignment, policy$flat$categories[[i]]$assignments))
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0('delete',label)]],{
            #update assign$table ????
            
            #delete any inner categories ????
            
            policy$categories <- deleteCategory(policy$categories, policy$flat, label) #if this remove button pressed, it deletes this category
            removeUI(
                selector = paste0("#cat",label) #this removes the UI for this category
            )
        }, ignoreInit = TRUE)
    }
#### -------------------------- ASSIGNMENTS ----------------------------#### 

    #keeps track of which category each assignment is assigned to
    assign <- reactiveValues(table = NULL)
    
    # creates assigns table when data uploads 
    # all assignments default to "Unassigned"
    observe({
        colnames <- get_assignments(data())
        assign$table <- data.frame(assignment = colnames, category = c("Unassigned"))
    })
    
    #a list of unassigned assignments in policies tab
    output$unassigned <- renderUI(
        if (!is.null(assign$table)){
            HTML(markdown::renderMarkdown(text = paste(paste0("- ", getUnassigned(assign$table), "\n"), collapse = "")))
        } else {
            h5("Let's upload some data first...")
        }
    )
    
#### -------------------------- EXCEPTIONS MODAL ----------------------------####   
    observeEvent(input$new_except, {
        showModal(exceptions_modal) #opens edit modal
        #updates values that aren't always the same but still default
        updateSelectInput(session, "cat_e", choices = get_cat_names(policy$flat))
        if (!is.null(assign$table)){
            updateSelectInput(session, "pick_id", choices = get_id_cols(data()))   
        }
        
    })
    
    observeEvent(input$pick_id,{
        updateSelectInput(session, "students", choices = data()[[input$pick_id]])
    })
    
    observeEvent(input$cat_e,{
        editing$name <- NULL
        i <- getIndex(policy$flat, input$cat_e)
        updateSelectInput(session, "aggregation", selected = policy$flat$categories[[i]]$aggregation)
        if (!is.null(policy$flat$categories[[i]]$weight)){
            shinyWidgets::updateAutonumericInput(session, "weight", value = policy$flat$categories[[i]]$weight)   
        }
        if (!is.null(policy$flat$categories[[i]]$n_drops)){
            updateNumericInput(session, "n_drops", value = policy$flat$categories[[i]]$n_drops*100)
        }
        if (!is.null(policy$flat$categories[[i]]$clobber)){
            updateSelectInput(session, "clobber", selected = policy$flat$categories[[i]]$clobber)
        }
        updateSelectizeInput(session, "assignments", selected = (policy$flat$categories[[i]]$assignments),
                             choices = c(assign$table$assignment, policy$flat$categories[[i]]$assignments))
    })
    
    observeEvent(input$cancel_e,{
        removeModal() #closes edit modal
    })
    
    observeEvent(input$save_e,{
        removeModal() #closes edit modal
        if (!is.null(editing$name)){
            # policy$exceptions <- updateCategory(policy$exceptions, policy$flat, editing$name, 
            #                                     input$name, input, assign$table)
        } else {
            policy$exceptions <- append(policy$exceptions, 
                                        list(createCategory(input$cat_e, input = input,
                                                            assign$table, exception = TRUE)))
        }
        #purrr::walk(names, rerender_ui)
        
    })
    
#### -------------------------- DASHBOARD ----------------------------####   
    output$download_policy <- downloadHandler(
        filename = function() {
            paste0(input$course_name, ".yml")
        },
        content = function(file) {
            write_yaml(list(coursewide = policy$coursewide,
                            categories = policy$categories), file)
        }
    )

     grades <- reactiveValues(scores = NULL)

    observe({
        
        cleaned_data <- data() |>
            drop_na(SID) |>
            group_by(SID) |>
            filter(row_number() == 1) |>
            ungroup()
        
        grades$scores <- cleaned_data
    })
    
    observeEvent(input$grading,{
        grades$scores <- grades$scores |>
            calculate_lateness(policy$flat) |>
            get_category_grades(policy$flat)
    })

    output$grading <- renderDataTable({ 
        datatable(grades$scores, options = list(scrollX = TRUE, scrollY = "500px"))
        })
    
    
#### -------------------------- SCRATCHPAD ----------------------------####   
    output$original_gs <- renderDataTable({
        datatable(data(), options = list(scrollX = TRUE, scrollY = "500px"))
    })
    
    output$assigns_table <- renderDataTable({ assign$table })
    
    #shows policy$categories in Scratchpad under policy_list tab
    output$policy_list <- renderPrint({
        Hmisc::list.tree(list(coursewide = policy$coursewide, 
                              categories = policy$categories, 
                              letter_grades = policy$letter_grades,
                              exceptions = policy$exceptions))
    })
    
    output$exceptions_print <- renderPrint({
        Hmisc::list.tree(policy$exceptions)
    })
    
    output$flat_policy <- renderPrint({
        Hmisc::list.tree(policy$flat)
    })
    
})

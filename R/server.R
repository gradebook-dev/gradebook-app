library(DT)
library(tidyverse)
#load helper scripts
HSLocation <- "helperscripts/"
source(paste0(HSLocation, "assignments.R"), local = TRUE)
source(paste0(HSLocation, "categories.R"), local = TRUE)
shinyServer(function(input, output, session) {
    
    #### -------------------------- UPLOADS ----------------------------####   
    
    #can only upload data that can be read in by read_gs()
    data <- reactive({
        req(input$upload)
        
        tryCatch({
            data <- gradebook::read_gs(input$upload$datapath)
            return(data)
        }, error = function(e) {
            showNotification('Please upload a file with the Gradescope format','',type = "error")
            return(NULL)
        })
    })
    
    #### -------------------------- POLICY ----------------------------####  
    policy <- reactiveValues(coursewide = list(course_name = "Course Name", description = "Description"),
                             categories = list(),
                             letter_grades = list(),
                             grades = NULL,
                             exceptions = list(),
                             flat = list())
    
    #### -------------------------- COURSEWIDE INFO ----------------------------####
    #modal to change saved course name + description
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
    
    output$course_name_display <- renderText({policy$coursewide$course_name})
    
    output$course_description_display <- renderText({policy$coursewide$description})
    
    
    #### -------------------------- ASSIGNMENTS ----------------------------#### 
    
    #keeps track of which category each assignment is assigned to, if any
    assign <- reactiveValues(table = NULL)
    
    # creates assigns table when data uploads 
    # all assignments default to "Unassigned"
    observe({
        colnames <- gradebook::get_assignments(data())
        assign$table <- data.frame(assignment = colnames) |>
            mutate(category = "Unassigned")
    })
    
    #a list of unassigned assignments in policies tab
    output$unassigned <- renderUI(
        if (!is.null(assign$table)){
            HTML(markdown::renderMarkdown(text = paste(paste0("- ", getUnassigned(assign$table), "\n"), collapse = "")))
        } else {
            h5("Let's upload some data first...")
        }
    )
    
    #### -------------------------- CATEGORIES MODAL ----------------------------####
    # if NULL, making new category; if not, editing category called editing$name
    editing <- reactiveValues(name = NULL,#to keep track of edited category
                              num = 0) #to create unique names for each new category
    
    
    # Opening category modal to create a NEW category
    observeEvent(input$new_cat, {
        editing$name <- NULL
        editing$num <- editing$num + 1 #increment to continue making unique category names
        showModal(edit_category_modal) #opens edit modal
        #updates values that aren't always the same but still default
        updateTextInput(session, "name", value = paste0("Category ", editing$num))
        if (!is.null(assign$table)){ #updates assignments if data has been loaded
            updateSelectizeInput(session, "assignments", choices = assign$table$assignment, selected = "")
        }
        
    })
    
    # Reactive Lateness Cells in Modal
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
    
    # Cancel and no changes will be made
    observeEvent(input$cancel,{
        removeModal() #closes edit modal
    })
    
    observeEvent(input$save,{
        removeModal() #closes edit modal
        
        # This is used to ensure no combination of subcategories and assignments
        # sum = 1 --> only assignments
        # sum = 0 --> only subcategories
        # sum is not an integer --> combination --> throws error notification
        sum <- 0
        if (!is.null(assign$table)){
            sum <- sum(input$assignments %in% assign$table[["assignment"]])/length(input$assignments) 
        }
        
        
        if (sum %in% c(0,1)){
            #update policy
            if (!is.null(editing$name)){
                #add new category
                policy$categories <- updateCategory(policy$categories, policy$flat, editing$name, 
                                                    input$name, input, assign$table)
            } else {
                policy$categories <- append(policy$categories, 
                                            list(createCategory(input$name, input = input,
                                                                assign$table)))
            }
        } else {
            showNotification('You cannot combine subcategories and assignments; please try again','',type = "error")
        }
        
        
    })
    
    #whenever policy$categories changes, policy$flat, assign$table and UI updates
    observe({
        policy$flat <- list(categories = policy$categories) |> gradebook::flatten_policy()
        assign$table <- updateAssignsTable(assign$table, policy$flat)
        # names <- purrr::map(policy$flat$categories, "category") |> unlist()
        # purrr::walk(names, rerender_ui)
    })
    
    #### -------------------------- GRADING ----------------------------####
    
    observeEvent(policy$categories,{
        if (!is.null(data()) & length(policy$categories) != 0){
            tryCatch({
                cleaned_data <- data() |>
                    drop_na(SID) |>
                    group_by(SID) |>
                    filter(row_number() == 1) |>
                    ungroup()
                
                flat_policy <- list(coursewide = policy$coursewide, 
                               categories = policy$categories, 
                               letter_grades = policy$letter_grades,
                               exceptions = policy$exceptions) |>
                    gradebook::flatten_policy()
                policy$grades <- cleaned_data |>
                    calculate_lateness(flat_policy) |>
                    get_category_grades(flat_policy)
            }, error = function(e) {
                showNotification('Fix policy file','',type = "error")
            })
        }
    })

    #### -------------------------- YAML ----------------------------####   
    
    output$download_policy_file <- downloadHandler(
        filename = function() {
            paste0(str_remove(policy$coursewide$course_name, "[^a-zA-Z0-9]"), ".yml")
        },
        content = function(file) {
            yaml::write_yaml(list(coursewide = policy$coursewide,
                            categories = policy$categories,
                            exceptions = policy$exceptions), file)
        }
    )
    
    #### -------------------------- DATA FILES ----------------------------####   
    # print out uploaded Gradescope data
    output$original_gs <- renderDataTable({
        datatable(data(), options = list(scrollX = TRUE, scrollY = "500px"))
    })
    
    #print out assignment table
    output$assigns_table <- renderDataTable({ assign$table })
    
    #shows policy$categories in Scratchpad under policy_list tab
    output$policy_list <- renderPrint({
        Hmisc::list.tree(list(coursewide = policy$coursewide, 
                              categories = policy$categories, 
                              letter_grades = policy$letter_grades,
                              exceptions = policy$exceptions))
    })
    
    output$flat_policy_list <- renderPrint({
        Hmisc::list.tree(policy$flat)
    })
    
    output$grades <- renderDataTable({ 
        datatable(policy$grades, options = list(scrollX = TRUE, scrollY = "500px"))
        })
})
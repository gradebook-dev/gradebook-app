library(DT)
library(tidyverse)
library(readr)
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
            choices <- getUnassigned(assign$table)
            updateSelectizeInput(session, "assignments", choices = choices, selected = "")
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
    
    observeEvent(input$edit, {
        editing$name <- input$edit_cat
        if (editing$name != ""){
            showModal(edit_category_modal) #opens edit modal
            label <- gsub(pattern = "[^a-zA-Z0-9]+", replacement = "", editing$name)
            i <- getIndex(policy$flat, label)
            updateTextInput(session, "name", value = editing$name)
            
            updateSelectInput(session, "aggregation", selected = policy$flat$categories[[i]]$aggregation)
            if (!is.null(policy$flat$categories[[i]]$weight)){
                shinyWidgets::updateAutonumericInput(session, "weight", value = policy$flat$categories[[i]]$weight*100)   
            }
            if (!is.null(policy$flat$categories[[i]]$n_drops)){
                updateNumericInput(session, "n_drops", value = policy$flat$categories[[i]]$n_drops)
            }
            if (!is.null(policy$flat$categories[[i]]$clobber)){
                updateSelectInput(session, "clobber", selected = policy$flat$categories[[i]]$clobber)
            }
            
            #update lateness
            if (!is.null(policy$flat$categories[[i]]$lateness)){
                num_lateness <- length(policy$flat$categories[[i]]$lateness)
                updateNumericInput(session, "num_lateness", value = num_lateness)
                # for (j in 1:num_lateness){
                #     late_policy <- policy$flat$categories[[i]]$lateness[[j]]
                #     updateTextInput(session, paste0("from", j), value = late_policy$from)
                #     updateTextInput(session, paste0("to", j), value = late_policy$to)
                #     updateTextInput(session, paste0("scale", j), value = late_policy$scale)
                # }
            }
            
            #update assignments
            choices <- c()
            if (!is.null(assign$table)){ #updates assignments if data has been loaded
                choices <- getUnassigned(assign$table)
            }
            selected = NULL
            if (!is.null(policy$flat$categories[[i]]$assignments)){
                selected <- policy$flat$categories[[i]]$assignments
                choices <- c(choices, selected)
            }
            updateSelectizeInput(session, "assignments", choices = choices, selected = selected)

        } else {
            showNotification("Please pick a category to edit", type = 'error')
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
        if (!is.null(assign$table) & !is.null(input$assignments)){
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
    
    
    observe({
        names <- purrr::map(policy$flat$categories, "category") |> unlist()
        if (!is.null(names)){
            updateSelectInput(session, "edit_cat", choices = names)
        } else {
            updateSelectInput(session, "edit_cat", choices = "")
        }
    })
    
    observeEvent(input$delete_cat, {
        if (input$edit_cat != ""){
            #some of this syntax is unnecessary now but will be relevant with dynamic UI
            editing$name <- input$edit_cat
            label <- gsub(pattern = "[^a-zA-Z0-9]+", replacement = "", editing$name)
            policy$categories <- deleteCategory(policy$categories, policy$flat, label)
        } else {
            showNotification("Please pick a category to delete",type = 'error')
        }
    })
    
    #whenever policy$categories changes, policy$flat, assign$table and UI updates
    observe({
        policy$flat <- list(categories = policy$categories) |> gradebook::flatten_policy()
        assign$table <- updateAssignsTable(assign$table, gradebook::flatten_policy(list(categories = policy$categories)))
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

    #### -------------------------- DOWNLOAD FILES ----------------------------####   
    
    output$download_policy_file <- downloadHandler(
        filename = function() {
            paste0(str_remove(policy$coursewide$course_name, "[^a-zA-Z0-9]"),"policy", ".yml")
        },
        content = function(file) {
            yaml::write_yaml(list(coursewide = policy$coursewide,
                            categories = policy$categories,
                            exceptions = policy$exceptions), file)
        }
    )
    
    output$download_grades <- downloadHandler(
        filename = function() {
            paste0(str_remove(policy$coursewide$course_name, "[^a-zA-Z0-9]"),"Grades", ".csv")
        },
        content = function(file) {
            readr::write_csv(policy$grades, file)
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
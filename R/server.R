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
    # data <- reactive({
    #     reg(input$upload_gs)
    #     
    #     tryCatch({
    #         data <- gradebook::read_gs(input$upload_gs$datapath)
    #         return(data)
    #     }, error = function(e) {
    #         showNotification('Please upload a file with the Gradescope format','',type = "error")
    #         return(NULL)
    #     })
    # })
    
    data <- reactive({
        if(is.null(input$upload_gs) && is.null(input$demogs) || input$demogs <= 0) {
            req(NULL)
        }
        data <- NULL
        if(!is.null(input$upload_gs)) {
            tryCatch({
                data <- gradebook::read_gs(input$upload_gs$datapath)
            }, error = function(e) {
                showNotification('Please upload a file with the Gradescope format', type = "error")
            })
        }
        if(input$demogs > 0 && is.null(data)) {
            data <- gradebook::gs_demo
        }
        return(data)
    })
    
    observe({
        req(input$upload_policy)
        #eventually validate
        tryCatch({
            yaml <- yaml::read_yaml(input$upload_policy$datapath)
            policy$coursewide <- yaml$coursewide
            policy$categories <- yaml$categories
        }, error = function(e) {
            showNotification('Please upload a policy file in YAML format','',type = "error")
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
        if (!is.null(assign$table$assignment)){
            HTML(markdown::renderMarkdown(text = paste(paste0("- ", getUnassigned(assign$table), "\n"), collapse = "")))
        } else {
            h5('New assignments will appear here.')
        }
    )
    
    #### -------------------------- CATEGORIES MODAL ----------------------------####
    current_edit <- reactiveValues(category = NULL)
    modalIsOpen <- reactiveVal(FALSE)
    
    # Opening category modal to create a NEW category
    observeEvent(input$new_cat, {
        showModal(edit_category_modal) #opens edit modal
        #updates values that aren't always the same but still default
        updateTextInput(session, "name", value = "Your Category name") #paste0("Category ", editing$num))
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
    
    observe({
        req(category_labels$edit)
        
        # Iterate over each category name to set up edit observers dynamically
        lapply(names(category_labels$edit), function(cat_name) {
            local({
                # Localize the variables to ensure they're correctly captured in the observer
                local_cat_name <- cat_name
                edit_id <- category_labels$edit[[local_cat_name]]
                
                observeEvent(input[[edit_id]], {
                    # Initialize a variable to hold the found category details
                    matched_category <- NULL
                    
                    # Iterate through policy$flat$categories to find a match
                    for (cat in policy$flat$categories) {
                        if (cat$category == cat_name) {  # Match found
                            matched_category <- cat
                            break
                        }
                    }
                    
                    if (!is.null(matched_category)) {
                        showModal(edit_category_modal) #opens edit modal
                        current_edit$category <- matched_category
                        cat_details <- matched_category
                        
                        updateTextInput(session, "name", value = cat_details$category)
                        updateSelectInput(session, "aggregation", selected = cat_details$aggregation)
                        shinyWidgets::updateAutonumericInput(session, "weight", value = cat_details$weight*100)   
                        updateNumericInput(session, "n_drops", value = cat_details$n_drops)
                        updateSelectInput(session, "clobber", selected = cat_details$clobber)
                        num_lateness <- length(cat_details$lateness)
                        updateNumericInput(session, "num_lateness", value = num_lateness)
                        # for (j in 1:num_lateness){
                        #     late_policy <- policy$flat$categories[[i]]$lateness[[j]]
                        #     updateTextInput(session, paste0("from", j), value = late_policy$from)
                        #     updateTextInput(session, paste0("to", j), value = late_policy$to)
                        #     updateTextInput(session, paste0("scale", j), value = late_policy$scale)
                        # }
                        
                        #update assignments
                        choices <- c()
                        if (!is.null(assign$table)){ #updates assignments if data has been loaded
                            choices <- getUnassigned(assign$table)
                        }
                        selected = NULL
                        if (!is.null(matched_category$assignments)){
                            selected <- matched_category$assignments
                            choices <- c(choices, selected)
                        }
                        updateSelectizeInput(session, "assignments", choices = choices, selected = selected)
                        } else {
                        showNotification("Please pick a category to edit", type = 'error')
                    }
                },         ignoreInit = TRUE)
            })
        })

    })   
    
    # Cancel and no changes will be made
    observeEvent(input$cancel,{
        removeModal() #closes edit modal
       
    })
    
    observeEvent(input$save,{
        existingCategories <- sapply(policy$flat$categories, function(cat) cat$category)
        if(input$name %in% existingCategories) {
            showNotification("Please enter a different category name. You cannot have repeating names. ", type = "error")
        }
        else{
            removeModal() #closes edit modal

            sum <- 0
            if (!is.null(assign$table) & !is.null(input$assignments)){
                sum <- sum(input$assignments %in% assign$table[["assignment"]])/length(input$assignments)
            }
            
            
            if (sum %in% c(0,1)){
                #update policy
                if (!is.null(current_edit$category$category)){
                    
                    #add new category
                    policy$categories <- updateCategory(policy$categories, policy$flat, current_edit$category$category,
                                                        input$name, input, assign$table)
                } else {
                    policy$categories <- append(policy$categories,
                                                list(createCategory(input$name, input = input,
                                                                    assign$table)))
                }
            } else {
                showNotification('You cannot combine subcategories and assignments; please try again','',type = "error")
            }
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
    
    category_to_be_deleted <- reactiveValues(cat = NULL)
    observe({
        req(category_labels$delete)

        # Iterate over each category name to set up edit observers dynamically
        lapply(names(category_labels$delete), function(cat_name) {
            local({
                # Localize the variables to ensure they're correctly captured in the observer
                local_cat_name <- cat_name
                delete_id <- category_labels$delete[[local_cat_name]]
                
                observeEvent(input[[delete_id]], {
                   
                    # Initialize a variable to hold the found category details
                    matched_category <- NULL
                    
                    # Iterate through policy$flat$categories to find a match
                    for (cat in policy$flat$categories) {
                        if (cat$category == cat_name) {  # Match found
                            matched_category <- cat
                            break
                        }
                    }
                    
                    if (!is.null(matched_category)) {
                        showModal(confirm_delete)
                        category_to_be_deleted$cat <- matched_category

                    } else {
                        showNotification("Please pick a category to delete",type = 'error')
                    }
                },ignoreInit = TRUE)
            })
        })
    })
    
    observeEvent(input$delete, {
        req(category_to_be_deleted$cat)
        removeModal()
       # print(category_to_be_deleted$cat$category)
        policy$categories <- deleteCategory(policy$categories, category_to_be_deleted$cat$category)
        category_to_be_deleted$cat <- NULL
    })

    #whenever policy$categories changes, policy$flat, assign$table and UI updates
    observe({
        policy$flat <- list(categories = policy$categories) |> gradebook::flatten_policy()
        assign$table <- updateAssignsTable(assign$table, gradebook::flatten_policy(list(categories = policy$categories)))
        # names <- purrr::map(policy$flat$categories, "category") |> unlist()
        # purrr::walk(names, rerender_ui)
    })
    
    #### -------------------------- DISPLAY CATEGORIES UI ----------------------------####
    
    category_labels <- reactiveValues(edit = list(), delete = list())
    
    
    observe({
        req(policy$flat$categories)
        category_levels <- assignLevelsToCategories(policy$flat$categories)
        result <- createNestedCards(policy$flat$categories, category_levels)
        
        output$categoriesUI <- renderUI({
            result$ui
        })
        
        # Store the labels returned from createNestedCards function
        category_labels$edit <- result$labels$edit
        category_labels$delete <- result$labels$delete
        
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
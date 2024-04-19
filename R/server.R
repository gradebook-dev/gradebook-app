library(DT)
library(gradebook)
library(tidyverse)
library(plotly)
library(bslib)
library(readr)
library(shinydashboard)

#load helper scripts
HSLocation <- "helperscripts/"
source(paste0(HSLocation, "assignments.R"), local = TRUE)
source(paste0(HSLocation, "categories.R"), local = TRUE)
source(paste0(HSLocation, "lateness.R"), local = TRUE)
shinyServer(function(input, output, session) {
    
    #### -------------------------- UPLOADS ----------------------------####   
    
    data <- reactiveVal(NULL)
    
    #can only upload data that can be read in by read_gs()
    observeEvent(input$upload_gs,{
        req(input$upload_gs)
        tryCatch({
            uploaded_data <- gradebook::read_gs(input$upload_gs$datapath)
            data(uploaded_data)
        }, error = function(e) {
            showNotification('Please upload a file with the Gradescope format','',type = "error")
            
        })
    })
    
    observeEvent(input$demogs, {
       if(is.null(data())){
        demo_data <- gradebook::gs_demo 
        data(demo_data)
       }
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
    
    # observe({
    #     req(input$demogs)
    #     tryCatch({
    #         yaml <- yaml::read_yaml("../inst/extdata/sample_policy.yaml")
    #         policy$coursewide <- yaml$coursewide
    #         policy$categories <- yaml$categories
    #     })
    # })
    # 
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
        if(length(colnames) > 0){
        assign$table <- data.frame(assignment = colnames) |>
            mutate(category = "Unassigned")
        }
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
        current_edit$category <- NULL
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
        existingCategories <- unlist(map(policy$flat$categories, "category"))
        if (!is.null(assign$table$assignment)){
            existingCategories <- c(existingCategories, gradebook::get_assignments(data()))
        }
        if (!is.null(current_edit$category)){
            existingCategories <- existingCategories[existingCategories != current_edit$category$category]
        }
        if(!is.null(existingCategories)  & input$name %in% existingCategories) {
            showNotification("Please enter a different category name. You cannot have repeating names. ", type = "error")
        }else{
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
    
    #### -------------------------- LATENESS POLICIES UI ----------------------------####
    
    # Opening category modal to create a NEW LATENESS
    observeEvent(input$new_lateness, {
        showModal(edit_lateness_modal) #opens lateness modal

    })
    
    #### -------------------------- ADVANCED LATENESS POLICIES UI ----------------------------####
    
    advanced_visible <- reactiveVal(FALSE)
    
    # Observe the toggle button
    observeEvent(input$advanced_toggle_lateness, {
        # Toggle the visibility
        advanced_visible(!advanced_visible())
    })
    
    # Render the advanced panel UI based on visibility
    output$advanced_lateness_policies_panel <- renderUI({
        if(advanced_visible()) {
            div(
                style = "margin-top: 20px; padding: 20px; background-color: #ffffff; border-radius: 5px;",
                h4("Advanced lateness policies here....")
            )
        }
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
                    gradebook::calculate_lateness(flat_policy) |>
                    gradebook::get_category_grades(flat_policy)
            }, error = function(e) {
                showNotification('Fix policy file','',type = "error")
            })
        }
    })
    
    #### -------------------------- DASHBOARD ----------------------------####
    
    output$dashboard <- renderUI({
        # if categories are made OR data is uploaded.
        if (length(policy$categories) > 0 && !is.null(assign$table$assignment)) {
            fluidRow(
                box(
                    tabsetPanel(
                        tabPanel('Plot', 
                            plotlyOutput('assignment_plotly', height = '220px')
                        ),
                        tabPanel('Statistics', 
                            # TODO
                            uiOutput('assignment_stats'),
                        ),
                    ),
                    width = 6,
                    height = '300px',
                ),
                box(
                    title = 'Assignment Options',
                    selectInput('which_assignment', label=NULL, choices = assign$table$assignment),
                    # TODO: radioButtons('assignment_score_option', 'Choose an option:', 
                    #              choices = list('Percentage' = 'percentage', 
                    #                             'By Points' = 'point'),
                    #              selected = 'percentage'),
                    width = 6,
                    height = '300px'
                    
                ),
                box(
                    tabsetPanel(
                        tabPanel('Plot', 
                            plotlyOutput('category_plotly', height = '220px'),
                        ),
                        tabPanel('Statistics', 
                            # TODO
                            uiOutput('category_stats', height = '200px'),
                        ),
                    ),
                    width = 6,
                    height = '300px'
                ),
                box(
                    title = 'Category Options', 
                    selectInput('which_category', label=NULL, choices = available_categories()),
                    # TODO: radioButtons('choice2', 'Choose an option:',
                    #              choices = list('Percentage' = 'percentage', 
                    #                             'By Points' = 'point'),
                    #              selected = 'percentage'),
                    width = 6,
                    height = '300px'
                ),
                box(
                    title = 'Overall Course Distribution',
                    plotlyOutput('overall_plotly', height = '320px'),
                    width = 12,
                    height = '400px'
                ),
                box(
                    DT::dataTableOutput('course_data_table'),
                    width = 12
                )
            )
        } else if (length(policy$categories) > 0) { # policy is created only
            tags$div(style = 'display: flex; flex-direction: column; justify-content: center; align-items: center; height: 60vh;',
                     tagList(
                         h4(strong('You haven\'t uploaded any student data yet.')),
                         h5('Upload course data from Gradescope to get started.')
                     )
            )
        } else if (!is.null(assign$table$assignment)) {
            tags$div(style = 'display: flex; flex-direction: column; justify-content: center; align-items: center; height: 60vh;',
                     tagList(
                         h4(strong('You still need to build your course policy.')),
                         h5('See "Policies" tab to get started.')
                     )
            )
        } else {
            tags$div(style = 'display: flex; flex-direction: column; justify-content: center; align-items: center; height: 60vh;',
                     tagList(
                         h4(strong('You haven\'t uploaded any student data yet.')),
                         h5('Summary statistics and plots will appear here as you build your course policy.')
                     )
            )
        }
    })

    output$assignment_plotly <- renderPlotly({
        assignment_grades <- policy$grades |>
            dplyr::select(input$which_assignment) |>
            dplyr::pull(1)
        
        # if (input$assignment_score_option == 'point') {
        #     assignment_grades
        # }
        
        plt <- plot_ly(x = ~assignment_grades, type='histogram') |>
            config(displayModeBar = FALSE) |>
            layout(
                title = list(text = 'Assignment Distribution', font = list(size = 14), y = 0.95),
                xaxis = list(title = 'percentage'),
                dragmode = FALSE
            )
        
        plt
    })
    
    output$assignment_stats <- renderUI({
        assignment_vec <- policy$grades |>
            dplyr::select(input$which_assignment) |> 
            drop_na() |>
            dplyr::pull(1)
        
        mu <- paste0((mean(assignment_vec) |> round(digits = 4)) * 100, '%')
        med <- paste0((median(assignment_vec) |> round(digits = 4)) * 100, '%')
        sd <- paste0((sd(assignment_vec) |> round(digits = 4)) * 100, '%')
        tfive <- paste0((quantile(assignment_vec, 0.25) |> round(digits = 4)) * 100, '%')
        sfive <- paste0((quantile(assignment_vec, 0.75) |> round(digits = 4)) * 100, '%')
        
        HTML(paste0(
            '<div style="display: flex; justify-content: space-between; border-bottom: 1px solid black; padding: 5px 0;"><p>Mean</p> <p>', mu, '</p></div>',
            '<div style="display: flex; justify-content: space-between; border-bottom: 1px solid black; padding: 5px 0;"><p>Standard Deviation</p> <p>', sd, '</p></div>',
            '<div style="display: flex; justify-content: space-between; border-bottom: 1px solid black; padding: 5px 0;"><p>Median</p> <p>', med, '</p></div>',
            '<div style="display: flex; justify-content: space-between; border-bottom: 1px solid black; padding: 5px 0;"><p>25%ile</p> <p>', tfive, '</p></div>',
            '<div style="display: flex; justify-content: space-between; padding: 5px 0;"><p>75%ile</p> <p>', sfive, '</p></div>'
        ))
    })
    
    output$category_plotly <- renderPlotly({
        category_grades <- policy$grades |>
            dplyr::select(input$which_category) |>
            dplyr::pull(1)
        
        plt <- plot_ly(x = ~category_grades, type = 'histogram') |>
            config(displayModeBar = FALSE) |>
            layout(
                title = list(text = 'Category Distribution', font = list(size = 14), y = 0.95),
                xaxis = list(title = 'percentage'),
                dragmode = FALSE
            )
        
        plt
    })
    
    output$category_stats <- renderUI({
        category_vec <- policy$grades |>
            dplyr::select(input$which_category) |> 
            drop_na() |>
            dplyr::pull(1)
        
        mu <- paste0((mean(category_vec) |> round(digits = 4)) * 100, '%')
        med <- paste0((median(category_vec) |> round(digits = 4)) * 100, '%')
        sd <- paste0((sd(category_vec) |> round(digits = 4)) * 100, '%')
        tfive <- paste0((quantile(category_vec, 0.25) |> round(digits = 4)) * 100, '%')
        sfive <- paste0((quantile(category_vec, 0.75) |> round(digits = 4)) * 100, '%')

        HTML(paste0(
            '<div style="display: flex; justify-content: space-between; border-bottom: 1px solid black; padding: 5px 0;"><p>Mean</p> <p>', mu, '</p></div>',
            '<div style="display: flex; justify-content: space-between; border-bottom: 1px solid black; padding: 5px 0;"><p>Standard Deviation</p> <p>', sd, '</p></div>',
            '<div style="display: flex; justify-content: space-between; border-bottom: 1px solid black; padding: 5px 0;"><p>Median</p> <p>', med, '</p></div>',
            '<div style="display: flex; justify-content: space-between; border-bottom: 1px solid black; padding: 5px 0;"><p>25%ile</p> <p>', tfive, '</p></div>',
            '<div style="display: flex; justify-content: space-between; padding: 5px 0;"><p>75%ile</p> <p>', sfive, '</p></div>'
        ))
    })
    
    output$overall_plotly <- renderPlotly({
        plt <- plot_ly(x = policy$grades$`Overall Score`, type = 'histogram') |>
            config(displayModeBar = FALSE) |>
            layout(dragmode = FALSE)
        plt
    })
    
    output$course_data_table <- DT::renderDataTable({ 
        # Removing max points column
        wanted_columns <- colnames(policy$grades)[!grepl('- Max Points', colnames(policy$grades))]
        tbl <- policy$grades |>
            select(wanted_columns)
        
        # Renaming columns for display purposes 
        names(tbl) <- gsub('\\(H:M:S\\)', '(Minutes)', names(tbl))
        
        # Rounding minutes to nearest tenth decimal place
        column_names <- grep('\\(Minutes\\)', names(tbl), value = TRUE)
        tbl[column_names] <- lapply(tbl[column_names], {function(x) round(x, 1)})
        
        DT::datatable(tbl, options = list(scrollX = TRUE, scrollY = '500px'))
    })
    
    available_categories <- reactive({
        return(sapply(policy$categories, {function(df) df$category}))
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
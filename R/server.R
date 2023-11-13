# load libraries
library(shinyWidgets)
library(DT)
library(tidyverse)
library(shiny.fluent)
library(shinyjs)

#load helper scripts
HSLocation <- "helperscripts/"
source(paste0(HSLocation, "categories.R"), local = TRUE)
source(paste0(HSLocation, "assignments.R"), local = TRUE)
source(paste0(HSLocation, "process-sid.R"), local = TRUE)
source(paste0(HSLocation, "grades.R"), local = TRUE)

shinyServer(function(input, output, session) {

#### -------------------------- UPLOAD A FILE ----------------------------#### 

    
    
    #testing
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
                options = list(scrollX = TRUE, scrollY = "500px")
            )
            
        }
    })
    
#### -------------------------- POLICY-COURSE NAME  ----------------------------####
    # initialize class_name and class_description as reactive values
    policy <- reactiveValues(coursewide = list(course_name = "Your Course Name",
                                               description = "This is the description of what the policy file is for and author/date info. 
                                              Any course-wide policies could go here (total slip days, total drops, letter grade cutoffs)"),
                             categories = list(),
                             cutoff = list(A = 90, B = 80, C = 70, D = 60, F = 0)
                             )
    #shows policy$categories in Scratchpad under policy_list tab
    output$policy_list <- renderPrint({
        Hmisc::list.tree(list(coursewide = policy$coursewide, categories = policy$categories, cutoff = policy$cutoff))
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
            if (!is.null(categories_df())){
            categories_df <- categories_df()
            updateSelectInput(session, "clobber_with", choices = c("None", categories_df[['name']]))
            }
        }
        
    }) 
    
    observeEvent(input$cancel, {
        removeModal()
    })
    
    updateModalValues <- function(edit_nr){
        #updated edit_category_modal with info from category with nr edit_num
        i <- getCatIndex(policy$categories, edit_nr)
        # passing a boolean value
        is_subcat_selected <- policy$categories[[i]]$is_subcat == "Yes"
        print(is_subcat_selected)
########### Toggle subcat not working!
        updateToggle.shinyInput(session, "subcat", value = is_subcat_selected)
###########       
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
            if (!is.null(categories_df())){
                categories_df <- categories_df()
                updateSelectInput(session, "clobber_with", choices = c("None", categories_df[['name']]))
            }
        }
        # Preload selected values
        preloaded_values <- policy$categories[[i]]$assigns
        if (any(preloaded_values != "None")){
            preloaded_values <- unlist(strsplit(preloaded_values, ", ")) # Split the string and unlist the result
            choices = c(choices, preloaded_values)
        }
        updateSelectizeInput(session,  inputId = "assign",  selected = strsplit(policy$categories[[i]]$assigns, ", ")[[1]])
        updateSelectizeInput(session,  inputId = "assign",  choices = choices, selected = preloaded_values)
    }
        
    
#### -------------------------- CATEGORY CARDS  ----------------------------#### 

    
    ####### DISABLE/ENABLE ON SUBCATEGORY TOGGLE
    
    observeEvent(input$subcat, {
        if (input$subcat) {
            disable("slip")
            disable("weight")
            disable("late_allowed1")
            disable("late_penalty1")
            disable("late_allowed2")
            disable("late_penalty2")
            disable("num_drops")
            disable("clobber_with")
        } else {
            enable("slip")
            enable("weight")
            enable("late_allowed1")
            enable("late_penalty1")
            enable("late_allowed2")
            enable("late_penalty2")
            enable("num_drops")
            enable("clobber_with")
        }
    })


    
    observeEvent(input$save, {

        i <- getCatIndex(policy$categories, editing$nr)
        original_name <- input$change_cat_name #if this is a new category
        if (i <= length(policy$categories)){ #if it's not a new category
            original_name <- policy$categories[[i]]$name 
        }
        
        policy$categories <- updateCategory(policy$categories, input, editing$nr)
        if (!is.null(assign$table)){
         assign$table <- updateAssigns(assign$table, input$assign, original_name, input$change_cat_name)
        }

        #adding my new assignment from the subcategory to a tibble
        if (input$subcat) {
            new_row <- tibble(subcat_colname = original_name)
            subcat_tibble$tibble <- bind_rows(subcat_tibble$tibble, new_row)
        }
        
        removeModal()
        editing$num <- editing$num + 1
        purrr::walk(policy$categories, rerender_ui)
    })
    
    
    #### -------------------------- #render category UI for policy page.  -------------------------- ######
    rerender_ui <- function(x) { #render category UI for policy page
        nr <- x$nr
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
                            h4(x$name),
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
       
        observeEvent(input[[paste0('edit',nr)]], {
            showModal(edit_category_modal) #opens edit modal
            editing$nr <- nr
            updateModalValues(nr)
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0('delete',nr)]],{
            i <- getCatIndex(policy$categories, nr)
            assign$table <- resetAssigns(assign$table, x$name)
            policy$categories <- deleteCategory(policy$categories, nr) #if this remove button pressed, it deletes this category
            removeUI(
                selector = paste0("#cat",nr) #this removes the UI for this category
            )
        })
    }
    
#### -------------------------- CREATE ASSIGNMENTS TABLE FROM SUBCATEGORIES  ----------------------------####  
    subcat_tibble <- reactiveValues(tibble = tibble(subcat_colname = NULL))
    
    

#### -------------------------- ASSIGNMENTS  ----------------------------####  
    #reactive unassigned assignments table
    assign <- reactiveValues(table = NULL)
    #update assignment with category x
    updateAssignTable <- function(x){
        assign$table <- updateAssigns(assign$table, x$assigns, x$name, x$name)
    }
    
   
    
    #takes reactive data output and creates a reactive assignment table
    #contains all the columns from the original dataframe(names, emails, all columns from assignments, etc)
    assignments <- reactive({
        data <- data()
        createAssignTable(data, subcat_tibble$tibble)
    })
    
    #creates unassigned assignments table, excludes all names, sections, latenes, etc...
    observe({
        data <- data()
        assign$table <- createAssignTable(data, subcat_tibble$tibble)%>%
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
                   across(contains("lateness"), convert_to_min), #function in process-sid
                   across(contains("lateness"), as.character))
        return(new_time)
    })
    
    output$new_data <- renderDataTable({
        datatable(new_data(),
        options = list(scrollX = TRUE, scrollY = "500px"))
    })
    
    processed_sids <- reactive({
        new_data <- new_data()
        process_sids(new_data)
    })
    #### MAKE A DATAFRAME WITH CATEGORIES #####
    categories_df <- reactive({
        map_df(policy$categories, ~{
            item <- .
            assigns <- paste(item$assigns, collapse = ", ")
            item$assigns <- assigns
            as.data.frame(t(unlist(item)))
        })
    })

    pivotdf <- reactive({
        processed_sids <- processed_sids()$unique_sids
        pivot(processed_sids, assign$table, categories_df())
    })
    
    output$pivotdf <- renderDataTable({
        datatable(
            pivotdf(),
            options = list(scrollX = TRUE, scrollY = "500px"))
        })

    #### -------------------------- GRADING ----------------------------####
    
    ### Step1: AllGradesTable calculations 
    category_grades <- reactive({
        if (!is.null(pivotdf()) && length(pivotdf()) > 0 && length(assign$table) > 0) {
            CategoryGrades(pivotdf()) 
        } 
    })
    output$all_grades_table <- renderDataTable({
        datatable(
            category_grades(),
            options = list(scrollX = TRUE, scrollY = "500px"))
    })
    
    ### Step2: GradesPerCategory calculations.
    gradespercategory <- reactive({
        if (!is.null(policy$categories) && length(category_grades()) > 0) {
            allgradestable <- category_grades()
            GradesPerCategory(allgradestable, policy$cutoff)
        } else {
            NULL
        }
    })
    output$grades_per_category <- renderDataTable({
        datatable(
        gradespercategory(),
        options = list(scrollX = TRUE, scrollY = "500px"))
    })
    
    ## GGPLOT in Coursewide - a plot about the GRADE BINS - A,B,C,D,F
    output$letter_dist <- renderPlot({
        if (!is.null(policy$categories) && length(gradespercategory()) > 0){
        plot <-gradespercategory() %>% ggplot(aes(x = as.integer(course_grade))) +geom_histogram() +
            geom_rug(alpha = .35) + labs(x = "Grade (out of 100)") + xlim(0, 110) +
            geom_vline(xintercept = as.numeric(policy$cutoff), color = "goldenrod", lwd = 1.5) +
            theme_minimal()
        return (plot)
        }
    })

    #renders grade bin UI
    output$grade_bin_percent <- renderUI({ grade_bin_pct()})
    
    #reactive grade bin text for percentages per letter grade
    grade_bin_pct <- reactive({
        if (!is.null(gradespercategory()) && ncol(gradespercategory()) > 3){
            tagList(
                fluidRow(
                    column(2,
                           h6(paste0(round(mean(gradespercategory()$course_letter_grade == "F")*100,2), " %"))
                    ),
                    column(2,
                           h6(paste0(round(mean(gradespercategory()$course_letter_grade == "D")*100,2), " %"))
                    ),
                    column(2,
                           h6(paste0(round(mean(gradespercategory()$course_letter_grade == "C")*100,2), " %"))
                    ),
                    column(2,
                           h6(paste0(round(mean(gradespercategory()$course_letter_grade == "B")*100,2), " %"))
                    ),
                    column(2,
                           h6(paste0(round(mean(gradespercategory()$course_letter_grade == "A")*100,2), " %"))
                    ),
                )
            )
        }
    })
    #reactively updates grade bins
    observe({
        policy$cutoff <- updateBins(policy$cutoff, input$A, input$B, input$C, input$D, input$F)
    })
    
    #### -------------------------- DASHBOARD ----------------------------####
    
    output$dashboard <- renderUI({
        if (length(policy$categories) > 0){     #if there are some categories
               fluidRow(
                   column(4,
                         
                          h3("Course Summary Statistics"),
                          HTML(course_stats_html()),
                          HTML(student_concerns_html()),
                          br()
                        
                          ),
                   column(8,
                              tabsetPanel(
                                  tabPanel("All Grades Distributions",
                                           br(),       #graph scores for overall final grades
                                           br(),
                                           plotOutput("grade_dist")
                                           ),
                                  tabPanel("Per Category",
                                           br(),       #graph scores for individual categories
                                           selectInput("which_cat", "Pick a Category", choices = unlist(purrr::map(policy$categories, "name"))),
                                           plotOutput("cat_dist")
                                           ),
                                  tabPanel("Per Assignment",
                                           br(),       #graph scores for individual assignments
                                           selectInput("which_assign", "Pick an Assignment", choices = assign$table$colnames),
                                           plotOutput("assign_dist")
                                  ),
                                  tabPanel("Grades Table",       #shows category and final grades
                                           br(),
                                           h6("If you would like to download your course grades, click the download button below."),
                                           downloadButton("download_grades_data"),
                                           br(),
                                           br(),
                                           h4("Overall Grades Table"),
                                           dataTableOutput("grades_per_category")
                                  )
                              )
                          )
                    )
        } else if (!is.null(assign$table)){     #if some data is uploaded
            tagList(
                #doesn't work
               # h3(paste0("Thank you! You have uploaded a dataset with ", nrow(assign$table), " assignments and ", nrow(grades), " students.")),
                h4("Go into the 'Policies' tab above and fill in the grading criteria for each category"),
                h4("or upload one of your courses from the left."),
                h4("Once you're done, return to this 'Dashboard' page to see your course statistics.")
            )
        } else {    #default message
            h4("Welcome to GradeBook! To begin, upload your Gradescope csv by clicking the 'Browse' button to the left")
        }
    })
    
    ### GGPLOT on a overall grades 
    output$grade_dist <- renderPlot({
        plot <- gradespercategory() %>% 
            mutate(Overall_Grade = as.integer(course_grade)) %>%
            ggplot(aes(x = Overall_Grade)) + geom_histogram( color = "grey", fill = "lightgrey") +
            ggtitle("Distribution of Overall Grades") + xlab("Individual Grades") +
            theme_bw()
        plot
    })
    
    ### GGPLOT on a distribution of a category of choice
    output$cat_dist <- renderPlot({
            plot <-gradespercategory() %>% ggplot(aes_string(x = input$which_cat)) +
                geom_histogram() + theme_bw() +
                ggtitle(paste0("Distribution of ", input$which_cat))
            return (plot)
    })
    
    ### GGPLOT on a distribution of an Assignment of choice
    output$assign_dist <- renderPlot({
        plot <-pivotdf() %>% 
            filter(colnames == input$which_assign)%>%
            mutate(score = raw_points/max_points) %>%
            ggplot(aes(x = score)) + geom_histogram() + theme_bw() +
            ggtitle(paste0("Distribution of ", input$which_assign))
        plot
    })
    
    #download grades table with category and overall grades
    output$download_grades_data <- downloadHandler(
        filename = function() {
            paste(policy$coursewide$course_name, Sys.Date(), ".csv", sep = "")
        },
        content = function(filename) {
            write.csv(gradespercategory(), filename, row.names = FALSE)
        })
    

    # output$download_policy_file <- downloadHandler(
    #     filename = function() {
    #         paste(policy$coursewide$course_name, Sys.Date(), ".R", sep = "")
    #     },
    #     content = function(file) {
    # 
    #         cat("policy_file <- ", deparse(policy$categories), file = file)
    #     },
    #     contentType = "text/plain"
    # 
    # )
    
    ### DOWNLLOAD .R POLICY FILE
    output$download_policy_file <- downloadHandler(
        filename = function() {
            paste(policy$coursewide$course_name, Sys.Date(), ".R", sep = "")
        },
        content = function(file) {
            deparsed_list <- deparse(policy$categories)
            formatted_list <- gsub(",", ",\n", deparsed_list)
            cat("policy_file <-", formatted_list, file = file)
        },
        contentType = "text/plain"
    )
    
    
    # Creates a reactive UI that shows the course statistics gives at least one category is added
    course_stats_html <- reactive({
        stats <- getGradeStats(gradespercategory())
        category_stats <- ""
        # for (i in 4:length(stats)) {
        #     if (!is.na(stats[i])) {
        #         category_stats <- paste0(category_stats, '<p class="card-text">',  names(stats)[i], stats[i], '</p>')
        #     }
        # }
        paste0(
            '<div class="card border-light mb-3">',
            '<div class="card-header">Course Stats</div>',
            '<div class="card-body">',
            '<p class="card-text">',stats[1],'</p>',
            '<p class="card-text">',stats[2],'</p>',
            '<p class="card-text">',stats[3],'</p>',
            category_stats,
            '</div>',
            '</div>'
        )
    })
    
    #returns a list of all failing students and their scores
    student_concerns_html <- reactive({
        student_concerns <- getStudentConcerns(gradespercategory())
        paste0(
            '<div class="card border-light mb-3">',
            '<div class="card-header">Students with Low Scores:</div>',
            '<div class="card-body">',
            '<ul style="padding-left: 0;">',
            '<ul>',
            paste(sapply(student_concerns, function(concern) {
                paste("<li>", concern, "</li>", sep = "")
            }), collapse = ""),
            '</ul>',
            '</div>',
            '</div>'
        )
    })
  
    
    #### -------------------------- JSON ----------------------------####
    path <- "../../gradebook-data"
    dir.create(path, showWarnings = FALSE)
    
    #make a df with json names and paths
    getJsonFiles <- function() {
        json_files <- list.files(path, pattern = "\\.json$", full.names = TRUE)# the regex pattern only gets the .json files in the category.
        json_names <- tools::file_path_sans_ext(basename(json_files)) #removes the path and .json extention elaving just the name
        # Return a list with the names and full paths of the JSON files
        return(list(Name = json_names, Path = json_files))
    }
    #initializea reactive value that stores the names and paths of the JSON files
    cat_json <- reactiveVal(getJsonFiles())

    #save config
    observeEvent(input$save_json, {
        # Create the file name and path
        name <- paste0(policy$coursewide$course_name, ".json")
        path_json <- file.path(path, name)
        #the data to be saved
        p_coursewide <- policy$coursewide
        p_categories <- policy$categories
        p_cutoffs <- policy$cutoff
        list <- list(p_coursewide, p_categories, editing$num, p_cutoffs)
        jsonlite::write_json(list, path_json, auto_unbox = TRUE, pretty = TRUE)
        
        #current number of JSON files
        len <- length(cat_json()$Name)
        # 1: Get the value of cat_json() into an R list
        cat_json_list <- cat_json()
        # 2: modify the list
        cat_json_list$Name[[len + 1]] <- policy$coursewide$course_name
        cat_json_list$Path[[len + 1]] <- path_json
        # 3: Update the whole reactive value with the new list
        cat_json(cat_json_list)
        # 4: Update cat_json reactive variable
        cat_json(getJsonFiles())
        
        updateSelectInput(session, "pick_policy", choices = cat_json()$Name)
   
         })
    #observe for changes to the JSON files and update the list of policies
    observe({
        json_files_names <- getJsonFiles()[["Name"]]
        if (length(json_files_names) == 0) {
            json_files_names <- "No policy files"
        }
        updateSelectInput(session, "pick_policy", choices = json_files_names)
    })
    observeEvent(input$upload_json, {
        if (input$pick_policy == "No policy files") {
            print("No policy files available in the directory")
            return()
        }
        
        json_files <- getJsonFiles()
        i <- which(input$pick_policy == json_files$Name)
        if (length(i) == 0) {
            print("Configuration not found")
            return()
        }
        if (file.exists(json_files$Path[[i]])) {
            df <- jsonlite::fromJSON(json_files$Path[[i]], simplifyVector = TRUE, simplifyDataFrame = FALSE)
            
            policy$coursewide <- df[[1]]
            policy$categories <- df[[2]]
            editing$num <- df[[3]]
            policy$cutoff <- df[[4]]
            purrr::walk(policy$categories, rerender_ui)
            updateNumericInput(session, "A", value = policy$cutoff$A)
            updateNumericInput(session, "B", value = policy$cutoff$B)
            updateNumericInput(session, "C", value = policy$cutoff$C)
            updateNumericInput(session, "D", value = policy$cutoff$D)
            updateNumericInput(session, "F", value = policy$cutoff$F)
            if (!is.null(assign$table)){
                purrr::walk(policy$categories, updateAssignTable)
            }
            
        } else {
            print("File not found")
        }
    })
    # Delete selected JSON file
    observeEvent(input$delete_json, {
        #show sweetalert confirmation dialog
        confirmSweetAlert(
            session = session,
            inputId = "delete_config_confirm",
            title = "Delete JSON File",
            text = "Are you sure you want to delete this JSON file?",
            type = "warning",
            showCancelButton = TRUE,
            cancelButtonText = "Cancel",
            confirmButtonText = "Delete",
            closeOnConfirm = TRUE,
            closeOnCancel = TRUE
        )
    })
    
    # Handle confirmation result
    observeEvent(input$delete_config_confirm, {
        if (input$delete_config_confirm) {
            #find chosen file
            selected_name <- input$pick_policy
            selected_files <- getJsonFiles()
            selected_path <- selected_files$Path[selected_files$Name == selected_name]
            
            #delete the JSON file and update the reactive value
            if (length(selected_path) > 0) {
                selected_path <- selected_path[1]
                
                if (file.exists(selected_path)) {
                    file.remove(selected_path)
                    # 1:get the value of cat_json() into an R list
                    cat_json_list <- cat_json()
                    # 2:modify list
                    cat_json_list$Name <- cat_json_list$Name[cat_json_list$Path != selected_path]
                    cat_json_list$Path <- cat_json_list$Path[cat_json_list$Path != selected_path]
                    # 3:update the whole reactive value with the new list
                    cat_json(cat_json_list)
                    updateSelectInput(session, "pick_policy", choices = cat_json()$Name)
                } else {
                    print("File not found")
                }
            } else {
                print("Selected policy not found")
            }
        }

})
    
    
    
 

})

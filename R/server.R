# load libraries
library(shinyWidgets)
library(DT)
library(tidyverse)
library(gradebook)

#load helper scripts
HSLocation <- "helperscripts/"
source(paste0(HSLocation, "categories.R"), local = TRUE)
source(paste0(HSLocation, "assignments.R"), local = TRUE)

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
    
#### -------------------------- CATEGORY MODAL ----------------------------####
    editing <- reactiveValues(num = 1) #used to give unique labels
    
    observeEvent(input$new_cat, {
        showModal(edit_category_modal) #opens edit modal
        #updates values that aren't always the same but still default
        updateTextInput(session, "name", value = paste0("Category ", editing$num))
        if (!is.null(assign$table)){ #updates assignments if data has been loaded
            updateSelectizeInput(session, "assignments", choices = assign$table$assignment, selected = "")
        }
        
    })
    
    observeEvent(input$cancel,{
        removeModal() #closes edit modal
    })
    
    observeEvent(input$save,{
        removeModal() #closes edit modal
        #update policy
        policy$categories <- append(policy$categories, 
                                    list(createCategory(input$name, input = input,
                                                   editing$num, assign$table)))
        #increment editing$num by 1
        editing$num <- editing$num + 1
        #update assign$table
        #TBD
    })
    
#### -------------------------- POLICY ----------------------------####  
    policy <- reactiveValues(coursewide = list(),
                             categories = list(),
                             letter_grades = list())
    
#### -------------------------- DISPLAY POLICY ----------------------------####
    
    output$syllabus <- renderUI({
        if (length(policy$categories) != 0){
            policy <- list(categories = policy$categories) |>
                gradebook::flatten_policy()
            nrs <- purrr::map(policy$categories, "nr") |> unlist()
            lapply(sort(nrs), function(nr) { 
                i <- which(nrs == nr)
                box(
                    title = policy$categories[[i]]$category, status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    "Weight: ", policy$categories[[i]]$weight, br(),
                    "Aggregation: ", policy$categories[[i]]$aggregation, br(),
                    "Assignments: ",paste(policy$categories[[i]]$assignments, collapse = ", "), br(),
                    actionButton(paste0('delete',nr), label = NULL, icon = icon("trash-can"),  style = "background-color: transparent; margin-right: 10px;"), #remove button for this category
                    actionButton(paste0('edit',nr), label = NULL, icon = icon("pen-to-square"), style = "background-color: transparent; "),
                    width = "100%"
                )
            } )
        }
        
    })
    
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
    
    
#### -------------------------- SCRATCHPAD ----------------------------####   
    output$original_gs <- renderDataTable({
        datatable(data(), options = list(scrollX = TRUE, scrollY = "500px"))
    })
    
    output$assigns_table <- renderDataTable({ assign$table })
    
    #shows policy$categories in Scratchpad under policy_list tab
    output$policy_list <- renderPrint({
        Hmisc::list.tree(list(coursewide = policy$coursewide, 
                              categories = policy$categories, 
                              letter_grades = policy$letter_grades))
    })
    
})

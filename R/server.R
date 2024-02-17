library(DT)

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
    
    #### -------------------------- DATA FILES ----------------------------####   
    # print out uploaded Gradescope data
    output$original_gs <- renderDataTable({
        datatable(data(), options = list(scrollX = TRUE, scrollY = "500px"))
    })
    
    #print out assignment table
    output$assigns_table <- renderDataTable({ assign$table })
})
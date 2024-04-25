###### --------------- LATENESS MODAL ------------- ####

edit_lateness_modal <- modalDialog(
    title = h4("Edit Lateness Policy"),
    textInput("policy_name", label = "Policy Name", value = "Policy Name"),
    fluidRow(
        column(width = 3,
               actionButton("add_interval", "Add Interval"),
        ),
        column(width = 3,
               actionButton("remove_interval", "Remove Last Interval"),
        )
    ),
    uiOutput("lateness_modal"),
    easyClose = TRUE,
    footer = tagList(
        modalButton("Cancel"),
        actionButton("save_lateness", "Save", style = "color: white; background-color: #337ab7;")
    )
)

###### --------------- LATENESS UI INSIDE MODAL ------------- ####

generate_lateness_ui <- function(lateness){
    renderUI({ 
        lapply(1:as.integer(lateness$num_late_cats), function(i) {
            fluidRow(
                column(width = 2, offset = 0,
                       selectInput(paste0("lateness_preposition", i), NULL,
                                   ifelse(i <= length(lateness$prepositions),
                                          lateness$prepositions[i],
                                          "Until"
                                   ),
                                   choices = c("Until", "After", "Between"))
                ),
                column(width = 3, offset = 0,
                       textInput(paste0("start", i), label = NULL,
                                 value = ifelse(i <= length(lateness$starts),
                                                lateness$starts[i],
                                                ""
                                 ),
                                 placeholder = "HH:MM:SS"),
                       #custom json to handle special time input
                       #file is saved in folder www
                       tags$head(includeScript("www/timeInputHandler.js"))
                ),
                column(width = 3, offset = 0,
                       conditionalPanel(
                           condition = paste0("input.lateness_preposition",i, "== 'Between'"),
                           textInput(paste0("end", i), label = NULL, 
                                     value = ifelse(i <= length(lateness$ends),
                                                    lateness$ends[i],
                                                    ""
                                     ),
                                     placeholder = "HH:MM:SS"),
                           #custom json to handle special time input
                           #file is saved in folder www
                           tags$head(includeScript("www/timeInputHandler.js"))
                       )
                ),
                column(width = 2, offset = 0,
                       selectInput(paste0("lateness_arithmetic", i), NULL, 
                                   ifelse(i <= length(lateness$arithmetics),
                                          lateness$arithmetics[i],
                                          "Add"
                                   ),
                                   choices = c("Add", "Scale_by", "Set_to"))
                ),
                column(width = 2, offset = 0,
                       numericInput(paste0("lateness_value", i), label = NULL,
                                    value = ifelse(i <= length(lateness$values),
                                                   lateness$values[i],
                                                   0.03
                                    )
                       )
                )
            )
        })
    })
}


###### --------------- LATENESS POLICIES UI ------------- ####


createLatenessCards <- function(lateness_table) {
    lapply(names(lateness_table), function(policy_name) {
        items <- lateness_table[[policy_name]]
        
        print("items")
        print(items)
        
        # content <- lapply(names(items), function(name) {
        #     # Type of lateness and its value
        #     policy_line <- paste(strong(name), ":", items[[name]])
        #     
        #     if (tolower(name) == "between") {
        #         # Special format for BETWEEN
        #         from_to_values <- items[[name]]
        #         
        #         policy_line <-  div(strong(name), " ", br(),
        #                             strong("From:"), from_to_values[["from"]], br(),
        #                             strong("To:"), from_to_values[["to"]], br(),
        #                             sep = " ")
        #         
        #         
        #     } else {
        #         
        #         policy_line <- paste(strong(name), ":", items[[name]])
        #     }
        #     
        #     HTML(policy_line)
        # })
        
        content <- lapply(names(items), function(name) {
            # Type of lateness and its value
            policy_line <- if (tolower(name) == "between") {
                # Special format for BETWEEN
                from_to_values <- items[[name]]
                tags$div(
                    tags$strong(ucfirst(name)), 
                    tags$br(),
                    tags$strong("From:"), from_to_values[["from"]], 
                    tags$br(),
                    tags$strong("To:"), from_to_values[["to"]],
                    tags$br()
                )
            } else {
                tags$div(
                    tags$strong(ucfirst(name)), ":", items[[name]], 
                    tags$br()
                )
            }
            policy_line
        })
        
        title <- div(
            class = "category-title",
            policy_name,
            actionButton(paste0('lateness_delete_', policy_name), label = NULL, icon = icon("trash"), style = "background-color: transparent; margin-right: 10px;"),
            actionButton(paste0('lateness_edit_', policy_name), label = NULL, icon = icon("edit"), style = "background-color: transparent;")
        )
        
        content <- do.call(tagList, content)
        
        box(
            title = title,
            status = "primary",
            width = 8,
            content
        )
    })
}

# Helper function to capitalize the first letter of each word
ucfirst <- function(s) {
    sapply(strsplit(s, " "), function(x) {
        paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " ")
    }, USE.NAMES = FALSE)
}

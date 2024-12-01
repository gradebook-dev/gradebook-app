###### --------------- SLIP DAYS MODAL ------------- ####


edit_slip_days_modal <- modalDialog(
    tags$head(
        tags$style(HTML('
                .help-icon {
                cursor: pointer;
            }
            .tooltip-box {
                display: none;
                position: absolute;
                background-color: #f9f9f9;
                border: 1px solid #d3d3d3;
                padding: 10px;
                width: 280px;
                border-radius: 5px;
                box-shadow: 0 2px 5px rgba(0,0,0,0.2);
                z-index: 100;
                font-weight: normal;
            }
            .help-icon:hover + .tooltip-box {
                display: block;
            }
            .custom-gear-btn {
                background-color: transparent;
                border: none;
                color: #007bff;
                font-size: 16px; 
                cursor: pointer;
                padding: 0px;
                vertical-align: middle;
            }
             .custom-gear-btn:hover {
                background-color: transparent;
             }
        '))
    ),
    title = h4("Edit Slip Days Policy"),
    fluidRow(column(6,offset = 0,
                    textInput("slip_days_policy_name", "Slip Days Name", value = "", width = "100%")
    ),
    column(6, offset = 0,
           selectInput('order',
                       label = div(style = "position:relative;", 
                                   tags$span("Order: ", style = "font-weight: bold;"),
                                   tags$i(class = "fas fa-info-circle help-icon"),
                                   tags$div(class = "tooltip-box",
                                            HTML("
                                                <ul>
                                                <li><b>Chronological:</b> The slip days are applied in chronological order and it is the default.  </li>
                                                <li><b>Given:</b> It allotts slip days based on the order given in the policy file in `assignments` under `slipdays` with no regard to the chronological order of these assignments.</li>
                                                </ul>
                                                    ")
                                   )
                       ),
                       selected = 'chronological',
                       choices = c(
                           'Chronological' = 'chronological',
                           'Given' = 'given'
                       )
           )
           
    )),
    selectizeInput("assignments", "Select Assignments:",
                   choices = "", multiple = TRUE, width = "100%",
                   options = list(create = TRUE)),
    uiOutput("slip_days_modal"),
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
                
                column(width = 3, offset = 0,
                       selectInput(paste0("lateness_preposition", i), NULL,
                                   ifelse(i <= length(lateness$prepositions),
                                          lateness$prepositions[i],
                                          "Until"
                                   ),
                                   choices = c("Until", "After", "Between"))
                ),
                column(width = 2, offset = 0,
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
                column(width = 2, offset = 0,
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
                column(width = 3, offset = 0,
                       selectInput(paste0("lateness_arithmetic", i), NULL, 
                                   ifelse(i <= length(lateness$arithmetics),
                                          lateness$arithmetics[i],
                                          "Add"
                                   ),
                                   
                                   choices = c('Add' = 'Add',
                                               'Scale By' = 'Scale_by', 
                                               'Set To' = 'Set_to'
                                   )
                                   
                       )
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


###### --------------- SLIP DAYS POLICIES UI ------------- ####

# 
# createLatenessCards <- function(lateness_table) {
#     lapply(names(lateness_table), function(late_policy_name) {
#         late_policy <- lateness_table[[late_policy_name]]
#         i <- 0.5
#         content <- lapply(late_policy, function(item) {
#             # Type of lateness and its value
#             i <<- i + 0.5
#             policy_line <- if (names(item) == "between") {
#                 # Special format for BETWEEN
#                 
#                 tags$div(
#                     tags$strong(paste0("INTERVAL ", i)),
#                     tags$br(),
#                     tags$strong(ucfirst(names(item))), 
#                     tags$br(),
#                     tags$strong("From:"), item$between$from,
#                     tags$br(),
#                     tags$strong("To:"), item$between$to,
#                     tags$br()
#                 )
#             } else if(names(item) %in% c("until", "after")){
#                 tags$div(
#                     tags$strong(paste0("INTERVAL ", i)),
#                     tags$br(),
#                     tags$strong(ucfirst(names(item))), ":", item,
#                     tags$br()
#                 )
#             } else {
#                 tags$div(
#                     tags$strong(ucfirst(names(item))), ":", item,
#                     tags$br()
#                 )
#             }
#             policy_line
#         })
#         
#         
#         icons <- div(
#             class = "category-title",
#             style = "text-align: right;", 
#             # late_policy_name,
#             actionButton(paste0('lateness_delete_', late_policy_name), label = NULL, icon = icon("trash"), style = "background-color: transparent; margin-right: 10px;"),
#             actionButton(paste0('lateness_edit_', late_policy_name), label = NULL, icon = icon("edit"), style = "background-color: transparent;")
#         )
#         
#         content <- do.call(tagList, content)
#         
#         box(
#             #title = title,
#             status = "primary",
#             width = 8,
#             icons,
#             content
#             
#         )
#     })
# }

# Helper function to capitalize the first letter of each word
# ucfirst <- function(s) {
#     sapply(strsplit(s, " "), function(x) {
#         paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " ")
#     }, USE.NAMES = FALSE)
# }
# 
# update_lateness <- function(policy_categories, cat_to_update, late_policy){
#     index <- find_indices(policy_categories, cat_to_update)
#     index_cmd <- paste0("policy_categories[[", paste(index, collapse = "]]$assignments[["), "]]$lateness <- late_policy")
#     eval(parse(text = index_cmd))
#     return(policy_categories)
# }

###### --------------- FUNCTION TO GENERATE A STRING FOR EACH POLICY ------------- ####
# format_policy <- function(policy) {
#     policy_strings <- c()
#     
#     # Loop through each item in the policy
#     for (interval in policy) {
#         # Handle the 'between' interval
#         if (!is.null(interval$between)) {
#             between_string <- sprintf("Between: FROM: %s TO: %s", interval$between$from, interval$between$to)
#             policy_strings <- c(policy_strings, between_string)
#         } 
#         
#         # Handle the 'until' interval
#         if (!is.null(interval$until)) {
#             until_string <- sprintf("Until: %s", interval$until)
#             policy_strings <- c(policy_strings, until_string)
#         }
#         
#         # Handle the 'after' interval
#         if (!is.null(interval$after)) {
#             after_string <- sprintf("After: %s", interval$after)
#             policy_strings <- c(policy_strings, after_string)
#         }
#         
#         # Handle the 'add' arithmetic 
#         if (!is.null(interval$add)) {
#             add_string <- sprintf("Add: %s", interval$add)
#             policy_strings <- c(policy_strings, add_string)
#         }
#         
#         # Handle the 'scale by' arithmetic 
#         if (!is.null(interval$scale_by)) {
#             scaleby_string <- sprintf("Scale By: %s", interval$scale_by)
#             policy_strings <- c(policy_strings, scaleby_string)
#         }
#         
#         # Handle the 'set to' arithmetic 
#         if (!is.null(interval$set_to)) {
#             setto_string <- sprintf("Set To: %s", interval$set_to)
#             policy_strings <- c(policy_strings, setto_string)
#         }
#     }
#     #does not create a new line...
#     paste(policy_strings, collapse = " ")
# }
###### --------------- DELETE SLIP DAYS POLICY ------------- ####
# confirm_delete_lateness <- modalDialog(
#     h4("Are you sure you want to delete this lateness policy ?"),
#     easyClose = TRUE,
#     footer = tagList(
#         actionButton("cancel", "Cancel"),
#         actionButton("delete_late", "Delete"))
#     
# )

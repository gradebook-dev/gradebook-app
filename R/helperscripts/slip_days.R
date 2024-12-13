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
    fluidRow(
        column(6,
               textInput("slip_days_policy_name", "Slip Days Policy Name", value = "", width = "100%")
        ),
        column(6,
               selectInput('slip_days_order',
                           label = div(style = "position:relative;", 
                                       tags$span("Order: ", style = "font-weight: bold;"),
                                       tags$i(class = "fas fa-info-circle help-icon"),
                                       tags$div(class = "tooltip-box",
                                                HTML("
                                                <ul>
                                                <li><b>Chronological:</b> Apply slip days in chronological order.</li>
                                                <li><b>Given:</b> Use the order specified in the policy file.</li>
                                                </ul>")
                                       )
                           ),
                           selected = 'chronological',
                           choices = c(
                               'Chronological' = 'chronological',
                               'Given' = 'given'
                           )
               )
        )
    ),
    fluidRow(
        column(6,
               numericInput("num_slip_days", "Number of Slip Days Allowed", value = 0, min = 0)
        )
    ),
    selectizeInput("assignments", "Select Assignments:", choices = NULL, multiple = TRUE, width = "100%"),
    easyClose = TRUE,
    footer = tagList(
        modalButton("Cancel"),
        actionButton("save_slip_days", "Save", style = "color: white; background-color: #337ab7;")
    )
)

###### --------------- SLIP DAYS POLICIES UI ------------- ####
createSlipDaysCards <- function(slip_days_table) {
    lapply(names(slip_days_table), function(policy_name) {
        slip_days_data <- slip_days_table[[policy_name]]
        title <- div(class = "category-title", 
                     slip_days_data$name,
                     actionButton(paste0("slip_days_delete_", slip_days_data$name), 
                                  label = NULL, 
                                  icon = icon("trash"), 
                                  style = "background-color: transparent; margin-right: 10px;"),
                     actionButton(paste0("slip_days_edit_", slip_days_data$name), 
                                  label = NULL, 
                                  icon = icon("edit"), 
                                  style = "background-color: transparent;")
                     
        )
        content <- tags$div(
            tags$strong("Number of Slip Days Allowed: "), slip_days_data$num_slip_days,
            tags$br(),
            tags$strong("Order: "), slip_days_data$order, 
            tags$br(),
            tags$strong("Assignments: "), paste(slip_days_data$assignments, collapse = ", ")
        )
        
        box(
            title = title,
            status = "primary",
            width = 8,
            content
        )
    })
}


###### --------------- DELETE SLIP DAYS POLICY ------------- ####
confirm_delete_slip_days <- modalDialog(
    h4("Are you sure you want to delete this slip days policy?"),
    easyClose = TRUE,
    footer = tagList(
        actionButton("cancel", "Cancel"),
        actionButton("delete_slip_days_confirm", "Delete")
    )
)


edit_lateness_modal <- modalDialog(
    title = h4("Edit Lateness Policy"),
    textInput("policy_name", "Policy Name", value = "Lateness Policy #1"),
    
    fluidRow(
        column(width = 3, offset = 0,
               selectInput("lateness_preposition", NULL, choices = c("Until", "After", "Between"))
        ),
        column(width = 3, offset = 0,
               textInput("lateness_time1", label = NULL, value = "", placeholder = "HH:MM:SS"),
               
        ),
        column(width = 3, offset = 0,
               selectInput("lateness_arithmetic", NULL, choices = c("Add", "Scale_by", "Set_to"))
        ),
        column(width = 3, offset = 0,
               numericInput("lateness_value", label = NULL, value = 0.03)
        )
    ),
    easyClose = TRUE,
    footer = tagList(
        modalButton("Cancel"),
        actionButton("save", "Save", style = "color: white; background-color: #337ab7;")
    )
)

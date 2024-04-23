edit_lateness_modal <- modalDialog(
    title = h4("Edit Lateness Policy"),
    textInput("policy_name", "Policy Name", value = "Lateness Policy #1"),
    fluidRow(
        column(width = 3,
               actionButton("add_interval", "Add Interval"),
               ),
        column(width = 3,
               actionButton("remove_interval", "Remove Last Interval"),
        )
    ),
    fluidRow(
        column(width = 2, offset = 0,
               selectInput("lateness_preposition1", NULL, choices = c("Until", "After", "Between"))
        ),
        column(width = 3, offset = 0,
               textInput("start1", label = NULL, value = "", placeholder = "HH:MM:SS"),
               #custom json to handle special time input
               #file is saved in folder www
               tags$head(includeScript("www/timeInputHandler.js"))
        ),
        column(width = 3, offset = 0,
               conditionalPanel(
                   condition = "input.lateness_preposition1 == 'Between'",
                   textInput("end1", label = NULL, value = "", placeholder = "HH:MM:SS"),
                   #custom json to handle special time input
                   #file is saved in folder www
                   tags$head(includeScript("www/timeInputHandler.js"))
               )
        ),
        column(width = 2, offset = 0,
               selectInput("lateness_arithmetic1", NULL, choices = c("Add", "Scale_by", "Set_to"))
        ),
        column(width = 2, offset = 0,
               numericInput("lateness_value1", label = NULL, value = 0.00)
        )
    ),
    uiOutput("lateness"),
    easyClose = TRUE,
    footer = tagList(
        modalButton("Cancel"),
        actionButton("save_lateness", "Save", style = "color: white; background-color: #337ab7;")
    )
)

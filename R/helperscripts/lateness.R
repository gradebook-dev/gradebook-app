edit_lateness_modal <- modalDialog(
    title = h4("Edit Lateness Policy"),
    
    textInput("policy_name", "Policy Name", value = "Lateness Policy #1"),
    
    fluidRow(
        column(3,
               h5("UNTIL:"),
        ),
        column(3,
               shinyWidgets::timeInput("time_until", label = NULL, value = "00:00:00")
        ),
        column(3,
               selectInput("penalty_type_until", "", choices = c("ADD", "SCALE_BY", "SET_TO")),
               
        ),
        column(3,
               numericInput("penalty_value_until", label = NULL, value = 0.03)
        )
    ),
    
    fluidRow(
        column(3,
               h5("AFTER:")),
        column(3,
               shinyWidgets::timeInput("time_after", label = NULL, value = "24:00:01")
        ),
        column(3,
               selectInput("penalty_type_after", "", choices = c("ADD", "SCALE_BY", "SET_TO")),
               
        ),
        column(3,
               numericInput("penalty_value_after", label = NULL, value = 0)
        )),
    
    fluidRow(
        column(3,
               h5("BETWEEN:")
               ),
        column(3, 
               shinyWidgets::timeInput("time_between_start", label = NULL, value = "00:00:01")
        ),
        column(3, 
               h5("and")),
        column(3,
               shinyWidgets::timeInput("time_between_end", label = NULL, value = "24:00:00")
        )
        ),
    fluidRow(
        column(3,
               h5(" ")
        ),
        column(3,
               selectInput("adjustment_type", "", choices = c("ADD", "SCALE_BY", "SET_TO"))
        ),
        
        column(3,
               numericInput("adjustment_value", label = NULL, value = 1)
        ),
        column(3,
               h5(" ")
        )
    ),
    
    footer = tagList(
        modalButton("Cancel"),
        actionButton("save", "Save", style = "color: white; background-color: #337ab7;")
    )
)

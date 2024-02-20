Policies <- tabItem(tabName = "policies",
                 h2("Create Your Policy File"),
                 ### COURSE NAME ###
                 fluidRow(
                     tagList(
                         div(style = "padding: 0px 20px 20px 20px;  background-color: white;",
                             div(
                                 
                                 style = "border: 1px solid #000; padding: 10px; border-radius: 5px; margin-top: 20px;",
                                 tags$div(
                                     style = "display: flex; justify-content: left; align-items: center;",
                                     tags$div(
                                         textOutput("course_name_display"),
                                         style = "font-size: 20px; display: inline-block; margin-right: 10px;"
                                     ),
                                     actionButton("edit_policy_name", label = NULL, icon = icon("pen-to-square"), style = "background-color: transparent; "),
                                     style = "border: none; background: transparent;"
                                     
                                 ),
                                 tags$div(
                                     textOutput("course_description_display"),
                                     style = "margin-top: 20px;"
                                 )
                             )
                         )
                     )
                 ),
                 
                 ### CATEGORIES ###
                 tabsetPanel(
                     tabPanel("Assignment View",
                              fluidRow(
                                  column(8,
                                         br(),
                                         fluidRow(
                                             column(1,
                                                    actionButton("new_cat", label = NULL, icon = icon("plus"), style = "background-color: transparent; margin-right: 10px;"),
                                                    ),
                                             column(6,
                                                    selectInput("edit_cat", "Delete this Category:", selected = NULL, choices = "")
                                                    ),
                                             column(1,
                                                    actionButton("delete_cat", label = NULL, icon = icon("trash-can"), style = "background-color: transparent; margin-right: 10px;"),
                                                    ),
                                             column(1,
                                                    actionButton("edit", label = NULL, icon = icon("pen-to-square"), style = "background-color: transparent; margin-right: 10px;"),
                                                    )
                                         ),
                                         verbatimTextOutput("policy_list"),
                                         tags$div(id='inputList') #this is all the dynamic UI for categories
                                  ),
                                  column(4,
                                         h4("New Assignments:"),
                                         uiOutput("unassigned")
                                  )
                              )
                     )
                 )
)
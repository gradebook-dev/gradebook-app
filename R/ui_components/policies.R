Policies <- tabItem(tabName = "policies",
                    icon = icon("filter"),
                    
                    ### COURSE NAME ###
                    fluidRow(
                        tagList(
                            div(style = "padding: 0px 20px 20px 20px;",
                                div(
                                    
                                    style = "border: 1px solid #000; padding: 10px; border-radius: 5px; margin-top: 20px;",
                                    tags$div(
                                        style = "display: flex; justify-content: left; align-items: center;",
                                        tags$div(
                                            textOutput("course_name_display"),
                                            style = "font-size: 20px; display: inline-block; margin-right: 10px;"
                                        ),
                                        actionButton("edit_policy_name", label = NULL, icon = icon("pen-to-square"), style = "background-color: transparent; "),
                                        actionGroupButtons(inputIds = c("save_json", "upload_json"), labels = c("Save this Course", "Upload your Course")),
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
                    fluidRow(
                        div(style = "padding: 0px 20px;",
                            tags$div(
                                style = "display: flex; justify-content: left; align-items: center;",
                                tags$div(
                                    tags$label("Categories", style = "font-weight: 400; font-size: 20px;"),
                                    style = "margin-right: 10px;"
                                ), 
                                verbatimTextOutput("editing"),
                                actionButton("new_cat", label = NULL, icon = icon("plus"), style = "background-color: transparent; margin-right: 10px;"),
                                br()
                            ))
                    ),
                    fluidRow(
                        column(8,
                               tags$div(id='inputList') #this is all the dynamic UI for categories
                               ),
                        column(4,
                               h4("New Assignments:"),
                               uiOutput("unassigned")
                               )
                    )
)

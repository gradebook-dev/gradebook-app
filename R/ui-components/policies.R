Policies <- tabItem(tabName = "policies",
                 h2("Create Policy File", style = "text-align: center;"),
                 ### COURSE NAME ###
                         fluidRow(
                             tagList(
                                 div(style = "margin-top: 0px; padding: 15px; background-color: #ffffff;",
                                     div(
                                         style = "border: 1px solid #ddd; padding: 20px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,.1);",
                                         tags$div(
                                             style = "display: flex; justify-content: left; align-items: center;",
                                             tags$div(
                                                 textOutput("course_name_display"),
                                                 style = "font-size: 24px; display: inline-block; margin-right: 10px; "
                                             ),
                                             actionButton("edit_policy_name", label = NULL, icon = icon("pen-to-square"), style = "margin-bottom: 10px; background-color: transparent;  color: #50A5EA;")
                                         ),
                                         tags$div(
                                             textOutput("course_description_display"),
                                             style = "margin-top: 20px; font-size: 16px;"
                                         )
                                     )
                                 )
                             )
                         ),
                 
                 ### CATEGORIES ###
               #  tabsetPanel(
                    # tabPanel("Policy View",
               fluidRow(
                  
                              tagList(
                                  div(style = "align-items: center; padding: 20px; background-color: #ffffff;",
                                      fluidRow(
                                          h4('Course Policy', style = " align-items: center;padding-left: 30px; font-size: 24px; display: inline-block; margin-right: 10px; " ),
                                          actionButton("new_cat", label = NULL, icon = icon("plus"), style = "margin-top: -5px;  background-color: transparent; margin-right: 10px; color: #50A5EA;"),
                                          hr(),
                                          ),
                              fluidRow(
                                  column(8, 
                                        # h4('Policy View', style = "font-size: 24px; display: inline-block; margin-right: 10px; " ),
                                         br(),
                                         fluidRow(style = "margin-left: 10px;"),
                                         uiOutput("categoriesUI"),
                                  ),
                                  column(4,
                                         h4("New Assignments:", style = "margin-bottom: 20px;"),
                                         uiOutput("unassigned")
                                  )
                              )
                     )
                 )))
#)
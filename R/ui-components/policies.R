Policies <- tabItem(tabName = "policies",
                 h2("Create Policy File", style = "text-align: center;"),
                 ### COURSE NAME ###
                         fluidRow(
                             tagList(
                                     div(
                                         class = 'policies-top-div policies-title-description',
                                         div(
                                             class = 'policies-title-and-button',
                                             tags$div(
                                                 textOutput("course_name_display"),
                                                 class = 'policies-title'
                                             ),
                                             actionButton("edit_policy_name", label = NULL, icon = icon("pen-to-square"), class = 'policies-top-button')
                                         ),
                                         div(
                                             textOutput("course_description_display"),
                                             class = 'policies-description'
                                         )
                                     )
                             )
                         ),
                 
                 ### CATEGORIES ###
               #  tabsetPanel(
                    # tabPanel("Policy View",
               fluidRow(
                      tagList(
                          div(class = 'policies-cards-div',
                              fluidRow(
                                  h4('Course Policy', class = 'policies-cards-title'),
                                  actionButton("new_cat", label = NULL, icon = icon("plus"), class = 'policies-cards-new-cat-button'),
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
                          )
                      )
)

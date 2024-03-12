library(plotly)
Dashboard <- tabItem(tabName = "dashboard",
                              fluidRow(
                                  column(8,
                                     h4("Dashboard"),
                                     tabsetPanel(
                                         tabPanel("Visualizer",
                                                  selectInput("granularity", label = h4("Granularity"),
                                                              choices = list("By Assignment" = 'by_assignment',
                                                                             "By Category" = "by_category"), 
                                                              selected = 1),
                                                  conditionalPanel(
                                                      condition = 'input.granularity' == 'by_assignment',
                                                      selectInput('assignment_choice', 'Choose Assignment',
                                                                  choices = list()) # TODO : get assignment names
                                                      ),
                                                  conditionalPanel(
                                                      condition = 'input.granularity' == 'by_category',
                                                      selectInput('categorical_choice', 'Choose Category',
                                                                  choices = list()) # TODO : get category names
                                                  ),
                                                  plotlyOutput("dashboardVisualizer", width="100%", height="50%"),
                                                  selectInput("binAdjustment", label = h4("Adjust Grade Bins "), 
                                                              choices = list("A" = 'aBin', "B" = "bBin", "C" = "cBin"), 
                                                              selected = 1)
                                         ),
                                         tabPanel("Breakdown",
                                                  dataTableOutput("dashboardBreakdown")
                                         )
                                     ),
                                     uiOutput("dashboardUI")
                                  ),
                                  column(4,
                                     br(),
                                     fluidRow(
                                         column(1,
                                                actionButton("asdf", label = NULL, icon = icon("plus"), style = "background-color: transparent; margin-right: 10px;"),
                                         ),
                                         uiOutput("dashboardOptions")
                                     ),
                                    
                                  )
                              )
)
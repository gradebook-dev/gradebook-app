Dashboard <- tabItem(tabName = "dashboard",
                              fluidRow(
                                  column(8,
                                     h4("Dashboard"),
                                     tabsetPanel(
                                         tabPanel("Visualizer",
                                                  plotlyOutput("dashboardVisualizer")
                                         ),
                                         tabPanel("Breakdown",
                                                  dataTableOutput("dashboardBreakdown")
                                         )
                                     ),
                                     selectInput("select", label = h4("Adjust Grade Bins "), 
                                                 choices = list("A" = 'aBin', "B" = "bBin", "C" = "cBin"), 
                                                 selected = 1),
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
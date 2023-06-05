scratchpad <- tabItem(tabName = "scratchpad",
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Original Data",
                                       dataTableOutput("input_data")
                              ),
                              tabPanel("cat_list",
                                       verbatimTextOutput("cat_list")
                              ),
                              tabPanel("assigns$table",
                                       dataTableOutput("assign")
                              ),
                              tabPanel("new_data()",
                                       dataTableOutput("new_data")),
                              tabPanel("pivotdf()",
                                       dataTableOutput("pivotdf"))
                              
                          )
                      )
)
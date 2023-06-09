scratchpad <- tabItem(tabName = "scratchpad",
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Original Data",
                                       dataTableOutput("input_data")
                              ),
                              tabPanel("policy_list",
                                       verbatimTextOutput("policy_list")
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
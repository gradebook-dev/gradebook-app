scratchpad <- tabItem(tabName = "scratchpad",
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Original Data",
                                       dataTableOutput("input_data")
                              ),
                              tabPanel("cat_list",
                                       tableOutput("cat_list")
                                       )
                              
                          )
                      )
)


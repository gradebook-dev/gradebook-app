scratchpad <- tabItem(tabName = "scratchpad",
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Original Data",
                                       dataTableOutput("input_data")
                              )
                          )
                      )
)


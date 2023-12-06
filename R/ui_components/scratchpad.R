scratchpad <- tabItem(tabName = "scratchpad",
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Original Data",
                                       dataTableOutput("original_gs"))
                          )
                      )
)
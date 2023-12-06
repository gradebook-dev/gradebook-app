scratchpad <- tabItem(tabName = "scratchpad",
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Original Data",
                                       dataTableOutput("original_gs")),
                              tabPanel("policy_list",
                                       verbatimTextOutput("policy_list")),
                              tabPanel("Assigns Table",
                                       dataTableOutput("assigns_table"))
                          )
                      )
)
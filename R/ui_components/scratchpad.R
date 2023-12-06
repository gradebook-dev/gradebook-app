scratchpad <- tabItem(tabName = "scratchpad",
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Original Data",
                                       dataTableOutput("original_gs")),
                              tabPanel("Policy File",
                                       verbatimTextOutput("policy_list")),
                              tabPanel("Flat Policy",
                                       verbatimTextOutput("flat_policy")),
                              tabPanel("Assigns Table",
                                       dataTableOutput("assigns_table"))
                          )
                      )
)
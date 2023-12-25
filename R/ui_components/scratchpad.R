scratchpad <- tabItem(tabName = "scratchpad",
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Original Data",
                                       dataTableOutput("original_gs")),
                              tabPanel("Policy File",
                                       verbatimTextOutput("policy_list")),
                              tabPanel("Flat Policy",
                                       verbatimTextOutput("flat_policy")),
                              tabPanel("Exceptions List",
                                       verbatimTextOutput("exceptions_print")),
                              tabPanel("Assigns Table",
                                       dataTableOutput("assigns_table")),
                              tabPanel("Grades",
                                       dataTableOutput("grading")
                                       )
                          )
                      )
)
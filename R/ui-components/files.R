Files <- tabItem(tabName = "files",
                 h2("Files"),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Original Gradescope Data",
                                  dataTableOutput("original_gs")
                                  ),
                         tabPanel("Assignments Table",
                                  dataTableOutput("assigns_table")
                         ),
                         tabPanel("Policy File",
                                  verbatimTextOutput("policy_list")
                                  ),
                         tabPanel("Flat Policy File",
                                  verbatimTextOutput("flat_policy_list")
                         )
                     )
                 )
)
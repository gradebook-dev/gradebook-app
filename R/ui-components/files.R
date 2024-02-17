Files <- tabItem(tabName = "files",
                 h2("Files"),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Original Gradescope Data",
                                  dataTableOutput("original_gs")
                                  ),
                         tabPanel("Assignments Table",
                                  dataTableOutput("assigns_table")
                         )
                     )
                 )
)
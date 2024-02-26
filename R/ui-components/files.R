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
                         tabPanel("Subcategories Tabl",
                                  dataTableOutput("subcat_table"),
                                  selectInput("print_subcat", "See a Subcat", 
                                              selected = NULL, choices = c()),
                                  verbatimTextOutput("print_out_subcat")
                                  ),
                         tabPanel("Flat Policy File",
                                  verbatimTextOutput("flat_policy_list")
                         ),
                         tabPanel("Final Grades",
                                  dataTableOutput("grades")
                                  )
                     )
                 )
)
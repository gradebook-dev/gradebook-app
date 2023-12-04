scratchpad <- tabItem(tabName = "scratchpad",
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Original Data",
                                       dataTableOutput("input_data")),
                              tabPanel("policy_list",
                                       verbatimTextOutput("policy_list")),
                              tabPanel("assigns$table",
                                       dataTableOutput("assign")),
                              tabPanel("pivotdf()",
                                       dataTableOutput("pivotdf")),
                              tabPanel("allgradestable()",
                                       dataTableOutput("all_grades_table")),
                              tabPanel("gradespercategory()",
                                      # dataTableOutput("grades_per_category")
                                      )
                          )
                      )
)
library(shinyWidgets)

Policies <- tabItem(tabName = "policies",
                     icon = icon("filter"),
                     fluidRow(
                         #for editing categories
                         column(6,
                                h4("Edit this Category"),
                                selectInput("cat_name", "Pick a Category", choices = c("Labs", "PS", "Quizzes")),
                                textInput("change_cat_name", "Enter Category Name", value = "", width = "100%"),
                                fluidRow(
                                    column(6,
                                           shinyWidgets::autonumericInput("weight", "Category Weight", value = "", currencySymbol = "%",
                                                                          currencySymbolPlacement = "s"),
                                           ),
                                    column(6,
                                           shinyWidgets::autonumericInput("num_drops", "How Many Drops?", value = "", currencySymbol = " drops",
                                                                          currencySymbolPlacement = "s", decimalPlaces = 0)
                                          
                                           )
                                ),
                                fluidRow(
                                    column(4,
                                           selectInput("grading_policy", strong("Aggregation Method"),
                                                        choices = c("Equally Weighted", "Weighted by Points")),
                                           selectInput("clobber_with", "Clobber with...",
                                                       choices = c("None", "Lab 1", "Lab 2", "Quiz 1", "Quiz 2", "PS 1", "PS 2"))
                                    ),
                                    column(4,
                                           textInput("late_allowed","First lateness policy?", placeholder = "enter as HH:MM:SS"),
                                           textInput("late_allowed2","Second lateness policy?", placeholder = "enter as HH:MM:SS")
                                           ),
                                    column(4,
                                           shinyWidgets::autonumericInput("late_penalty", "Deduction?", value = "", currencySymbol = "%",
                                                                          currencySymbolPlacement = "s"),
                                           shinyWidgets::autonumericInput("late_penalty2", "Deduction?", value = "", currencySymbol = "%",
                                                                          currencySymbolPlacement = "s")
                                           )
                                ),
                                selectizeInput("assign", "Select Assignments:",
                                               choices = c("Lab 1", "Lab 2", "Quiz 1", "Quiz 2", "PS 1", "PS 2"),
                                               multiple = TRUE,
                                               width = "100%"),
                                actionGroupButtons(inputIds = c("delete", "save", "new"), 
                                                   labels = c("Delete", "Save", "New Category"))
                                ),
                         #for category cards
                         column(6,
                                selectizeInput("assign", "Select Assignments:",
                                               choices = '',
                                               multiple = TRUE,
                                               width = "100%")
                                ) 
                     )
)
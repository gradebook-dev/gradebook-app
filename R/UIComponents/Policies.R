library(shinyWidgets)

Policies <- tabItem(tabName = "policies",
                     icon = icon("filter"),
                     fluidRow(
                         #for editing categories
                         column(6,
                                h4("Edit this Category"),
                                selectInput("cat_name", "Pick a Category", choices = c("Labs", "PS", "Quizzes")),
                                textInput("change_cat_name", "Enter Category Name", value = "", width = "100%"),
                                shinyWidgets::autonumericInput("weight", "What is this category weighted?", value = "", currencySymbol = "%",
                                                               currencySymbolPlacement = "s"),
                                fluidRow(
                                    column(4,
                                           radioButtons("grading_policy", strong("Aggregation Method"),
                                                        choices = c("Equally Weighted", "Weighted by Points")),
                                           numericInput("num_drops", "How Many Drops:", 0, step = 1),
                                           radioButtons("clobber_boolean", strong("Is there a clobber policy?"),
                                                        choices = c("Yes", "No"),
                                                        selected = "No"),
                                           conditionalPanel(
                                               condition = "input.clobber_boolean == 'Yes'",
                                               selectInput("clobber_with", "Clobber with...",
                                                           choices = c("Lab 1", "Lab 2", "Quiz 1", "Quiz 2", "PS 1", "PS 2")))
                                    ),
                                    column(8,
                                           radioButtons("late_boolean", strong("Is there a lateness policy?"),
                                                        choices = c("Yes", "No"),
                                                        selected = "No"),
                                           conditionalPanel(
                                               condition = "input.late_boolean == 'Yes'",
                                               fluidRow(
                                                   column(6,
                                                          textInput("late_allowed","Allowed lateness?", placeholder = "enter as HH:MM:SS")
                                                   ),
                                                   column(6,
                                                          shinyWidgets::autonumericInput("late_penalty", "Deduction?", value = "", currencySymbol = "%",
                                                                                         currencySymbolPlacement = "s")
                                                   ),
                                                   radioButtons("late_boolean2", strong("Is there another lateness policy?"),
                                                                choices = c("Yes", "No"),
                                                                selected = "No"),
                                                   conditionalPanel(
                                                       condition = "input.late_boolean2 == 'Yes'",
                                                       fluidRow(
                                                           column(6,
                                                                  textInput("late_allowed2","Allowed lateness?", placeholder = "enter as HH:MM:SS")
                                                           ),
                                                           column(6,
                                                                  shinyWidgets::autonumericInput("late_penalty2", "What percent is deducted?", value = "", currencySymbol = "%",
                                                                                                 currencySymbolPlacement = "s")
                                                           )
                                                       )
                                                   )
                                               )
                                           )
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
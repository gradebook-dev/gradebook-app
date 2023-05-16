library(shinyWidgets)

Policies <- tabPanel("Policies",
                     icon = icon("filter"),
                     fluidRow(
                         #for editing categories
                         column(6,
                                h4("Edit this Category"),
                                selectInput("cat_name", "Pick a Category", choices = c("Labs", "PS", "Quizzes")),
                                textInput("change_cat_name", "Enter Category Name", value = "", width = "75%"),
                                shinyWidgets::autonumericInput("weight", "What is this category weighted?", value = "", currencySymbol = "%",
                                                               currencySymbolPlacement = "s")
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
edit_category_modal <- modalDialog(
    
                tags$head(
                    tags$style(
                        HTML("
                            .spacing > * + * {
                                margin-left: 10px;
                            }
                            .custom-flex-container {
                                display: flex;
                                align-items: center;
                                margin-right: 5px;
                            }

                        ")
                    )
                ),
               h4("Edit this Category"),
               fluidRow(
                   column(6,
                          selectInput("cat_name", "Category", choices = c("Labs", "PS", "Quizzes"))
                   ),
                   column(6,
                          textInput("change_cat_name", "Category Name", value = "", width = "100%")
                   )
               ),
               fluidRow(column(6,
                               div(
                                   style = "display: flex; flex-direction: column;",
                                   h4("Lateness"),
                                   div(
                                       style = "display: flex; align-items: center;",
                                       class = "spacing",
                                       tags$label("Slip Days"),
                                       div(
                                           style = "display: flex; align-items: center;",
                                           class = "custom-flex-container",
                                           shinyWidgets::autonumericInput("slip", label = "", value = "", width = "50px", decimalPlaces = 0)
                                       )
                                   ),
                                   div(
                                       style = "display: flex; align-items: center;",
                                       class = "spacing",
                                       tags$label("After"),
                                       textInput("late_allowed1", "", placeholder = "HH:MM:SS", width = "100px"),
                                       tags$label("scale by:"),
                                       shinyWidgets::autonumericInput("late_penalty1", "", value = "",
                                                                      currencySymbolPlacement = "s", width = "50px")
                                   ),
                                   div(
                                       style = "display: flex; align-items: center;",
                                       class = "spacing",
                                       tags$label("After"),
                                       textInput("late_allowed2", "", placeholder = "HH:MM:SS", width = "100px"),
                                       tags$label("scale by:"),
                                       shinyWidgets::autonumericInput("late_penalty2", "", value = "",
                                                                      currencySymbolPlacement = "s", width = "50px")
                                   )
                                )
                    ),
                       column(6,
                              div(style = "display: flex; flex-direction: column;",
                                  h4("Additional Categories"),
                                  div(
                                        style = "display: flex; align-items: center;",
                                        class = "spacing",
                                        tags$label("Weight"),
                                        shinyWidgets::autonumericInput("weight", "", value = "", currencySymbol = "%",
                                                                            currencySymbolPlacement = "s", width = "100px"),
                                        tags$label("Drops?"),
                                        shinyWidgets::autonumericInput("num_drops", "", value = "", currencySymbol = " drops",
                                                                            currencySymbolPlacement = "s", decimalPlaces = 0,width = "100px")
                                      ),
                                  div(
                                      style = "display: flex; align-items: center; margin-top: -5px;",
                                      class = "spacing",
                                      tags$label("Aggregation"),
                                      selectInput("grading_policy", strong(""),
                                                  choices = c("Equally Weighted", "Weighted by Points"))
                                      ),
                                  div(
                                      style = "display: flex; align-items: stretch; align-items: center; margin-top: -15px;",
                                      class = "spacing",
                                      tags$label("Clobber with..."),
                                      selectInput("clobber_with", "",
                                                  choices = c("None", "Lab 1", "Lab 2", "Quiz 1", "Quiz 2", "PS 1", "PS 2")
                                                  )
                                  )
                               )
               )),
               selectizeInput("assign", "Select Assignments:",
                              choices = c("Lab 1", "Lab 2", "Quiz 1", "Quiz 2", "PS 1", "PS 2"),
                              multiple = TRUE,
                              width = "100%"),
               fluidRow(
                   column(6,
                          checkboxInput("as_assign", strong("Save as Aggregated Assignments"), value = FALSE)
                   ),
                   column(6,
                          
                   )
               ),
               footer = tagList(
                   modalButton("Cancel"),
                   actionButton("save", "Save")
               )

)

new_category_modal <- modalDialog(
    
    title = "Create New Category",
    textInput("category_name_input", "Category Name", value = ""),
    footer = tagList(
        modalButton("Cancel"),
        actionButton("add_category_name", "Save"),
    )
)
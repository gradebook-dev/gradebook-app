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
               textInput("change_cat_name", "Category Name", value = "", width = "100%")
        ),
        column(6,
               
               tags$label(""),
               
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
                                autonumericInput("slip", label = "", value = 0, width = "50px", decimalPlaces = 0)
                            )
                        ),
                        div(
                            style = "display: flex; align-items: center;",
                            class = "spacing",
                            tags$label("After"),
                            textInput("late_allowed1", "", value = "00:00:00", placeholder = "HH:MM:SS", width = "100px"),
                            tags$label("scale by:"),
                            autonumericInput("late_penalty1", "", value = 0,
                                             currencySymbolPlacement = "s", width = "50px")
                        ),
                        div(
                            style = "display: flex; align-items: center;",
                            class = "spacing",
                            tags$label("After"),
                            textInput("late_allowed2", "", value = "00:00:00", placeholder = "HH:MM:SS", width = "100px"),
                            tags$label("scale by:"),
                            autonumericInput("late_penalty2", "", value = 0,
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
                   shinyWidgets::autonumericInput("weight", "", value = 0, currencySymbol = "%",
                                                  currencySymbolPlacement = "s", width = "100px"),
                   tags$label("Drops?"),
                   autonumericInput("num_drops", "", value = 0, currencySymbol = " drops",
                                    currencySymbolPlacement = "s", decimalPlaces = 0,width = "100px")
               ),
               div(
                   style = "display: flex; align-items: center; margin-top: -5px;",
                   class = "spacing",
                   tags$label("Aggregation"),
                   selectInput("grading_policy", strong(""), selected = "Equally Weighted",
                               choices = c("Equally Weighted", "Weighted by Points"))
               ),
               div(
                   style = "display: flex; align-items: stretch; align-items: center; margin-top: -15px;",
                   class = "spacing",
                   tags$label("Clobber with..."),
                   selectInput("clobber_with", "", selected = "None", choices = c("None"))
               )
           )
    )),
    selectizeInput("assign", "Select Assignments:",
                   choices = "",
                   multiple = TRUE,
                   options = list(delimiter = ','),
                   width = "100%"),
    fluidRow(
        column(6,
               checkboxInput("as_assign", strong("Save as Aggregated Assignments"), value = FALSE)
        ),
        column(6,
               
        )
    ),
    footer = tagList(
        actionButton("cancel", "Cancel"),
        actionButton("save", "Save")
    )
    
)

getCatIndex <- function(cat_list, edit_nr){
    nrs <- purrr::map(cat_list, "nr") |>
        unlist()
    i <- which(nrs == edit_nr)
    if (length(i) == 0){
        i <- length(cat_list)+1
    }
    return (i)
}

#deletes category "cat_name"
deleteCategory <- function(cat_list, edit_num){
   
}

# updates category "cat_name" with input data
updateCategory <- function(cat_list, input, edit_nr){
    #create default category
    edit_num <- unlist(strsplit(edit_nr, "cat"))[2]
    assignments <- ifelse(length(input$assign) == 0, "None", paste(input$assign, collapse = ", "))
    category <- list(name = ifelse(input$change_cat_name == "", paste0("Category ", edit_num), input$change_cat_name),
                         slip_days = ifelse(length(input$slip) == 0, 0, input$slip),
                         late_time1 = ifelse(input$late_allowed1 == "", "00:00:00", input$late_allowed1),
                         late_time2 = ifelse(input$late_allowed2 == "", "00:00:00", input$late_allowed2),
                         late_scale1 = ifelse(length(input$late_penalty1) == 0, 0, input$late_penalty1),
                         late_scale2 = ifelse(length(input$late_penalty2) == 0, 0, input$late_penalty2),
                         weight = ifelse(length(input$weight) == 0, 0, input$weight),
                         drops = ifelse(length(input$num_drops) == 0, 0, input$num_drops),
                         aggregation = ifelse(length(input$grading_policy) == 0, "Equally Weighted", input$grading_policy),
                         clobber = ifelse(length(input$clobber_with) == 0, "None", input$clobber_with),
                         assigns = ifelse(length(input$assign) == 0, "None", paste(input$assign, collapse = ", ")),
                         nr = edit_nr
    )
    i <- getCatIndex(cat_list, edit_nr)
    cat_list[[i]] <- category
    return (cat_list)
}


update_ui_categories <- function(cat_list, id) {
    i <- which(cat_list$nr == id)
    tagList(
        div(
            style = "padding: 0px 20px 20px 20px;",
            tags$div(
                style = "justify-content: left; align-items: center;",
                tags$div(
                    div(
                        style = "display: flex; align-items: center;",
                        paste("Slip Days: ", cat_list$slip_days[i]),
                        paste("Weight: ", cat_list$weight[i]),
                        paste("Drops: ", cat_list$drops[i]),
                        paste("Clobber Policy: ", cat_list$clobber[i]),
                        paste("Assignments Included: ", cat_list$assigns[i])
                    )
                )
            )
        )
    )
}
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
                                autonumericInput("slip", label = "", value = "", width = "50px", decimalPlaces = 0)
                            )
                        ),
                        div(
                            style = "display: flex; align-items: center;",
                            class = "spacing",
                            tags$label("After"),
                            textInput("late_allowed1", "", placeholder = "HH:MM:SS", width = "100px"),
                            tags$label("scale by:"),
                            autonumericInput("late_penalty1", "", value = "",
                                             currencySymbolPlacement = "s", width = "50px")
                        ),
                        div(
                            style = "display: flex; align-items: center;",
                            class = "spacing",
                            tags$label("After"),
                            textInput("late_allowed2", "", placeholder = "HH:MM:SS", width = "100px"),
                            tags$label("scale by:"),
                            autonumericInput("late_penalty2", "", value = "",
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
                   autonumericInput("num_drops", "", value = "", currencySymbol = " drops",
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

# added default category to end of cat_list
addCategory <- function(cat_list, edit_num){
    #create default category
    new_category <- list(name = paste0("New Category ", edit_num),
                     slip_days = 0,
                     late_time1 = "00:00:00",
                     late_time2 = "00:00:00",
                     late_scale1 = 0,
                     late_scale2 = 0,
                     weight = 0,
                     drops = 0,
                     aggregation = "Equally Weighted",
                     clobber = "None",
                     assigns = "",
                     nr = paste0("cat", edit_num)
                     )
    cat_list[[length(cat_list)+1]] <- new_category
    return (cat_list)
}

getCatIndex <- function(cat_list, edit_num){
    nrs <- purrr::map(cat_list, "nr") |>
        unlist()
    i <- which(nrs == paste0("cat", edit_num))
}

#deletes category "cat_name"
deleteCategory <- function(cat_list, edit_num){
    i <- getCatIndex(cat_list, edit_num)
    cat_list <- cat_list[-i]
    
    return(cat_list)
}

# updates category "cat_name" with input data
updateCategory <- function(cat_list, input, cat_name){
    i <- getCatIndex(cat_list, edit_num)
    #if no input, sets to default values
    cat_list[[i]]$name <- ifelse(input$change_cat_name == "", paste0("Category ", i), input$change_cat_name)
    cat_list[[i]]$slip_days <- ifelse(length(input$slip) == 0, 0, input$slip)
    cat_list[[i]]$late_time1 <- ifelse(input$late_allowed1 == "", "00:00:00", input$late_allowed1)
    cat_list[[i]]$late_time2 <- ifelse(input$late_allowed2 == "", "00:00:00", input$late_allowed2)
    cat_list[[i]]$late_scale1 <- ifelse(length(input$late_penalty1) == 0, 0, input$late_penalty1)
    cat_list[[i]]$late_scale2 <- ifelse(length(input$late_penalty2) == 0, 0, input$late_penalty2)
    cat_list[[i]]$weight <- ifelse(length(input$weight) == 0, 0, input$weight)
    cat_list[[i]]$drops <- ifelse(length(input$num_drops) == 0, 0, input$num_drops)
    cat_list[[i]]$aggregation <- ifelse(length(input$grading_policy) == 0, "Equally Weighted", input$grading_policy)
    cat_list[[i]]$clobber <- ifelse(length(input$clobber_with) == 0, "None", input$clobber_with)

    #adding assignemtns to the cat_list[[i]]
    assignments <- ifelse(length(input$assign) == 0, "None", paste(input$assign, collapse = ", "))
    cat_list[[i]]$assigns[i] <- assignments

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
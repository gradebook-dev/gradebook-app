edit_category_modal <- modalDialog(
    
    tags$head(
        tags$style(
            HTML(".spacing > * + * {
                                margin-left: 10px;
                            }
                            .custom-flex-container {
                                display: flex;
                                align-items: center;
                                margin-right: 5px;
                            }
                 "))),
    h4("Edit this Category"),
    fluidRow(column(6,textInput("change_cat_name", "Category Name", value = "", width = "100%")
            ),
            column(6,tags$label(""),
            )),
    fluidRow(column(6,div(
                        style = "display: flex; flex-direction: column;",
                        h4("Lateness"),
                        div(style = "display: flex; align-items: center;",
                            class = "spacing",
                            tags$label("Slip Days"),
                            div(style = "display: flex; align-items: center;",
                                class = "custom-flex-container",
                                autonumericInput("slip", label = "", value = 0, width = "50px", decimalPlaces = 0))
                        ),
                        div(style = "display: flex; align-items: center;",
                            class = "spacing",
                            tags$label("Until"),
                            textInput("late_allowed1", "", value = "00:00:00", placeholder = "HH:MM:SS", width = "100px"),
                            tags$label("scale by:"),
                            autonumericInput("late_penalty1", "", value = 0,
                                             currencySymbolPlacement = "s", width = "50px")
                        ),
                        div(style = "display: flex; align-items: center;",
                            class = "spacing",
                            tags$label("Until"),
                            textInput("late_allowed2", "", value = "00:00:00", placeholder = "HH:MM:SS", width = "100px"),
                            tags$label("scale by:"),
                            autonumericInput("late_penalty2", "", value = 0,
                                             currencySymbolPlacement = "s", width = "50px")
                        ))),
            column(6,div(style = "display: flex; flex-direction: column;",
                       h4("Additional Categories"),
                       div(style = "display: flex; align-items: center;",
                           class = "spacing",
                           tags$label("Weight"),
                           shinyWidgets::autonumericInput("weight", "", value = 0, currencySymbol = "%",
                                                          currencySymbolPlacement = "s", width = "100px"),
                           tags$label("Drops?"),
                           autonumericInput("num_drops", "", value = 0, currencySymbol = " drops",
                                            currencySymbolPlacement = "s", decimalPlaces = 0,width = "100px")
                       ),
                       div(style = "display: flex; align-items: center; margin-top: -5px;",
                           class = "spacing",
                           tags$label("Aggregation"),
                           selectInput("grading_policy", strong(""), selected = "Equally Weighted",
                                       choices = c("Equally Weighted", "Weighted by Points"))
                       ),
                       div(style = "display: flex; align-items: stretch; align-items: center; margin-top: -15px;",
                           class = "spacing",
                           tags$label("Clobber with..."),
                           selectInput("clobber_with", "", selected = "None", choices = c("None"))
                       )))),
            selectizeInput("assign", "Select Assignments:",
                           choices = "",
                           multiple = TRUE,
                           options = list(delimiter = ','),
                           width = "100%"),
    fluidRow(column(6,
               checkboxInput("as_assign", strong("Save as Aggregated Assignments"), value = FALSE)),
            column(6,)),
    footer = tagList(
            actionButton("cancel", "Cancel"),
            actionButton("save", "Save"))
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
deleteCategory <- function(cat_list, edit_nr){
    i <- getCatIndex(cat_list, edit_nr)
    cat_list <- cat_list[-i]
}

# updates category "cat_name" with input data
updateCategory <- function(cat_list, input, edit_nr){
    #create default category
    edit_num <- unlist(strsplit(edit_nr, "cat"))[2]
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
                         assigns = input$assign,
                         num_assigns = length(input$assign),
                         #assigns = ifelse(length(input$assign) == 0, "None", as.vector(input$assign)),
                         nr = edit_nr
    )
    if (length(input$assign) == 0){
        category$assigns <- "None"
    }
    i <- getCatIndex(cat_list, edit_nr)
    cat_list[[i]] <- category
    return (cat_list)
}

update_ui_categories <- function(cat_list, nr) {
    i <- getCatIndex(cat_list, nr)
    
    assignments <- stringr::str_c(cat_list[[i]]$assigns, collapse = ", ") 
    tagList(
        div(style = "border-left: 1px solid #ddd; padding-left: 10px; display: flex;",
            div(
                style = "flex: 1; display: flex; flex-direction: column; margin-right: 10px;",
                p(strong("Weight:"), style = "margin-bottom: 5px;"),
                p(strong("Drops:"), style = "margin-bottom: 5px;"),
                p(strong("Grading Policy:"), style = "margin-bottom: 5px;"),
                p(strong("Clobber Policy:"), style = "margin-bottom: 5px;"),
                p(strong("Slip Days:"), style = "margin-bottom: 5px;"),
                p(strong("Assignments Included:"), style = "margin-bottom: 5px;")
            ),
            div(style = "flex: 1; display: flex; flex-direction: column;",
                p(paste(cat_list[[i]]$weight), style = "margin-bottom: 5px;"),
                p(paste(cat_list[[i]]$drops), style = "margin-bottom: 5px;"),
                p(paste(cat_list[[i]]$aggregation), style = "margin-bottom: 5px;"),
                p(paste(cat_list[[i]]$clobber), style = "margin-bottom: 5px;"),
                p(paste(cat_list[[i]]$slip_days), style = "margin-bottom: 5px;"),
                p(paste(assignments), style = "margin-bottom: 5px;")
    )))
}
edit_category_modal <- modalDialog(

    h4("Edit this Category"),
    fluidRow(column(6,textInput("name", "Category Name", value = "", width = "100%")
            )),
    h6("Insert More Criteria Here Later..."),
    fluidRow(
        column(6,
               selectInput("aggregation", "Aggregation:", selected = "equally_weighted",
                           choices = c("equally_weighted", "weighted_by_points", 
                                       "max_score", "min_score", "none"))),
        column(6,
               shinyWidgets::autonumericInput("weight", "", value = 0, currencySymbol = "%",
                                              currencySymbolPlacement = "s", width = "100px"))
    ),
    selectizeInput("assignments", "Select Assignments:",
                   choices = "", multiple = TRUE, width = "100%",
                   options = list(create = TRUE)),
    
    footer = tagList(
            actionButton("cancel", "Cancel"),
            actionButton("save", "Save"))
    )

createCategory <- function(name, input, editing_num, assigns_table){
    assignments = c()
    
    if (length(input$assignments != 0)){
        i = 1
        for (assign in input$assignments){
            if (assign %in% assigns_table$assignment){
                assignments = append(assignments, assign)
            } else{
                sub_cat <- createEmptyCategory(assign, editing_num, i)
                assignments = append(assignments, list(sub_cat))
                i = i+1
                
            }
        }
    }
    
    list(
        category = name,
        aggregation = input$aggregation,
        weight = input$weight,
        nr = editing_num,
        assignments = assignments
    )
}

createEmptyCategory <- function(name, editing_num, i){
    list(category = name,
         aggregation = "none",
         weight = 0,
         assignments = NULL,
         nr = paste(editing_num, i, sep = "-")
         )
}

getIndex <- function(flat_policy, nr){
    nrs <- purrr::map(flat_policy$categories, "nr") |> unlist()
    which(nrs == nr)
}
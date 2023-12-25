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
        column(3,
               shinyWidgets::autonumericInput("weight", "Weight:", value = 0, currencySymbol = "%",
                                              currencySymbolPlacement = "s", width = "100px"),
               numericInput("num_lateness", label = "Number of Lateness Intervals:", value = 0)
               ),
        column(3,
               numericInput("n_drops", label = "Number of Drops:", value = 0, min = 0),
               selectInput("clobber", "Clobber with:", selected = "None", choices = "")
               )
    ),
    selectizeInput("assignments", "Select Assignments:",
                   choices = "", multiple = TRUE, width = "100%",
                   options = list(create = TRUE)),
    uiOutput("lateness"),
    
    footer = tagList(
            actionButton("cancel", "Cancel"),
            actionButton("save", "Save"))
    )

createCategory <- function(name, input, assigns_table){
    assignments = c()
    
    if (length(input$assignments != 0)){
        i = 1
        for (assign in input$assignments){
            if (assign %in% assigns_table$assignment){
                assignments = append(assignments, assign)
            } else{
                sub_cat <- createEmptyCategory(assign, i)
                assignments = append(assignments, list(sub_cat))
                i = i+1
                
            }
        }
    }
    
    lateness <- NULL
    
    if (input$num_lateness > 0){
        lateness <- list()
        for (i in 1:as.integer(input$num_lateness)){
            lateness <- append(lateness, list(
                from = input[[paste0("from", i)]],
                to = input[[paste0("to", i)]],
                scale = input[[paste0("scale", i)]]
            ))
        }
        
        return (list(
            category = name,
            aggregation = input$aggregation,
            weight = input$weight,
            n_drops = input$n_drops/100,
            clobber = input$clobber,
            lateness = lateness,
            assignments = assignments
        ))
    }
    
    list(
        category = name,
        aggregation = input$aggregation,
        weight = input$weight,
        n_drops = input$n_drops/100,
        clobber = input$clobber,
        assignments = assignments
    )
}



createEmptyCategory <- function(name, i){
    list(category = name,
         aggregation = "none",
         weight = 0,
         n_drops = 0,
         clobber = "None",
         assignments = NULL)
}

updateCategory <- function(policy_categories, flat_policy, original_name, name, input, assigns_table){
    category <- createCategory(name, input, assigns_table) #needs to be updated
    original_name <- flat_policy$categories[[getIndex(flat_policy, original_name)]]$category
    index <- find_indices(policy_categories, original_name)
    index <- paste0("policy_categories[[",paste(index, collapse = "]]$assignments[["), "]]")
    print(index)
    eval(parse(text = paste(index, "<-", "category")))
    return (policy_categories)
}

deleteCategory <- function(policy_categories, flat_policy, label){
    if (length(getIndex(flat_policy, label)) > 0){
        name <- flat_policy$categories[[getIndex(flat_policy, label)]]$category
        index <- find_indices(policy_categories, name)
        index <- paste0("policy_categories[[",paste(index, collapse = "]]$assignments[["), "]]")
        eval(parse(text = paste(index, "<-", "NULL")))
    }
    return (policy_categories)
}

find_indices <- function(lst, target, current_index = c()) {
    indices <- c()
    
    for (i in seq_along(lst)) {
        if (is.list(lst[[i]]) && !is.null(lst[[i]]$category) && identical(lst[[i]]$category, target)) {
            indices <- c(current_index, i)
            break
        } else if (is.list(lst[[i]]) && !is.null(lst[[i]]$assignments)) {
            indices <- find_indices(lst[[i]]$assignments, target, c(current_index, i))
        }
        
        if (length(indices) > 0) {
            break
        }
    }
    
    if (length(indices) == 0) {
        return(NULL)
    }
    
    return(indices)
}
getIndex <- function(flat_policy, name){
    names <- purrr::map(flat_policy$categories, "category") |> unlist() |>
        gsub(pattern = "[^a-zA-Z0-9]+", replacement = "")
    name <- gsub(pattern = "[^a-zA-Z0-9]+", replacement = "", name)
    which(names == name)
}
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
               selectInput("clobber", "Clobber with:", selected = "None", choices = c("None"))
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
    
    #logic: if assignment appears in assignment table, add as assignment in policy file
    #       if assignment isn't in assign$table, create subcategory
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
    
    category <- list(
        category = name,
        aggregation = input$aggregation
    )
    
    if (input$num_lateness > 0){
        lateness <- list()
        for (i in 1:as.integer(input$num_lateness)){
            late_policy <- list(
                from = input[[paste0("from", i)]],
                to = input[[paste0("to", i)]],
                scale = input[[paste0("scale", i)]]
            )
            lateness <- append(lateness, list(late_policy))
        }
        
        category <- append(category, list(lateness = lateness))
    }
    
    if (input$clobber != "None"){
        category <- append(category, list(clobber = input$clobber))
    }
    
    if (input$weight != 0){
        weight <-  input$weight/100
        category <- append(category, list(weight = input$weight/100))
    }
    
    if (input$n_drops > 0){
        category <- append(category, list(n_drops = input$n_drops))
    }
    
    return (append(category, list(assignments = assignments)))
    
}



createEmptyCategory <- function(name, i){
    list(category = name,
         aggregation = "none",
         assignments = NULL)
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

updateCategory <- function(policy_categories, flat_policy, original_name, name, input, assigns_table){
    category <- createCategory(name, input, assigns_table) #needs to be updated
    original_name <- flat_policy$categories[[getIndex(flat_policy, original_name)]]$category
    index <- find_indices(policy_categories, original_name)
    index <- paste0("policy_categories[[",paste(index, collapse = "]]$assignments[["), "]]")
    eval(parse(text = paste(index, "<-", "category")))
    return (policy_categories)
}

getIndex <- function(flat_policy, name){
    names <- purrr::map(flat_policy$categories, "category") |> unlist() |>
        gsub(pattern = "[^a-zA-Z0-9]+", replacement = "")
    name <- gsub(pattern = "[^a-zA-Z0-9]+", replacement = "", name)
    which(names == name)
}
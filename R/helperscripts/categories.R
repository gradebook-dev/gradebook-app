library(shinyBS)
edit_category_modal <- modalDialog(
    
    h4("Edit this Category"),
    fluidRow(column(6,offset = 0,
                    textInput("name", "Category Name", value = "", width = "100%")
    )),
    fluidRow(
        column(6,offset = 0,
               selectInput('aggregation',
                           label = tags$span('Aggregation:', bsButton('aggregation_info', label = '', icon = icon('info'), size = 'extra-small')),
                           selected = 'equally_weighted',
                           choices = c('Equally Weighted' = 'equally_weighted',
                                          'Weighted By Points' = 'weighted_by_points', 
                                          'Max Score' = 'max_score',
                                          'Min Score' = 'min_score',
                                          'None' = 'none'
                                       )
               ),
            bsPopover(
               id = 'aggregation_info',
               title = 'hey',
               content = paste0(
                   'Equally Weighted: Weighs all assignments in category equally.'
               ),
               # TODO
               placement = 'right',
               trigger = 'hover'
            ),
            selectInput("lateness_policies", "Lateness Policy", selected = "None", choices = c("None"))
        ),
        column(3,offset = 0,
               shinyWidgets::autonumericInput("weight", "Weight:", value = 0, currencySymbol = "%",
                                              currencySymbolPlacement = "s"),
               numericInput("n_drops", label = "Number of Drops:", value = 0, min = 0)
               
        )
    ),
    selectizeInput("assignments", "Select Assignments:",
                   choices = "", multiple = TRUE, width = "100%",
                   options = list(create = TRUE)),
    fluidRow(
        column(6,offset = 0,
               div(h4('Advanced', style = " align-items: center; font-size: 16px; display: inline-block; margin-right: 0px; " ),
                   actionButton("advanced_toggle_lateness", label = NULL, icon = icon("gear"), 
                                style = "background-color: transparent; border: none; color: #50A5EA; margin-top: -5px;"),
                   
                   uiOutput("advanced_lateness_policies_panel")
                   
               )
        )
    ),
    easyClose = TRUE,
    footer = tagList(
        actionButton("cancel", "Cancel"),
        actionButton("save", "Save"))
)

confirm_delete <- modalDialog(
    h4("Are you sure you want to delete this category ?"),
    easyClose = TRUE,
    footer = tagList(
        actionButton("cancel", "Cancel"),
        actionButton("delete", "Delete"))
    
)

createCategory <- function(name, input, assigns_table){
    #with the current implementation of the R Package, order matters
    #order should be: category, lateness, drops, aggregation, assignments
    #other defaults (e.g. score, aggregation_max_pts, etc) are added by validate_policy()
    
    assignments = c()
    #logic: if assignment appears in assignment table, add as assignment in policy file
    #       if assignment isn't in assign$table, create subcategory
    if (length(input$assignments) != 0){
        for (assign in input$assignments){
            if (assign %in% assigns_table$assignment){
                assignments = append(assignments, assign)
            } else{
                sub_cat <- createEmptyCategory(assign)
                assignments = append(assignments, list(sub_cat))
                
            }
        }
    }
    
    category <- list(
        category = name
    )
    
    # if (input$num_lateness > 0){
    #     lateness <- list()
    #     for (i in 1:as.integer(input$num_lateness)){
    #         late_policy <- list(
    #             from = input[[paste0("from", i)]],
    #             to = input[[paste0("to", i)]],
    #             scale = input[[paste0("scale", i)]]
    #         )
    #         lateness <- append(lateness, list(late_policy))
    #     }
    # 
    #     category <- append(category, list(lateness = lateness))
    # }

    # if (input$clobber != "None"){
    #     category <- append(category, list(clobber = input$clobber))
    # }
    # 
    # if (input$weight != 0){
    #     weight <-  input$weight/100
    #     category <- append(category, list(weight = input$weight/100))
    # }
    # 
    if (input$n_drops > 0){
        category <- append(category, list(drop_n_lowest = input$n_drops))
    }
    
    return (append(category, list(aggregation = input$aggregation,
                                  assignments = assignments)))
    
}



createEmptyCategory <- function(name){
    list(category = name,
         aggregation = "equally_weighted",
         assignments = NULL)
}


deleteCategory <- function(policy_categories, matched_category_name) {
    # Using find_indices function to directly find the category to delete
    index <- find_indices(policy_categories, matched_category_name)
    if (length(index) > 0) {
        # Constructing the dynamic command to set the category or its assignments to NULL
        index_cmd <- paste0("policy_categories[[", paste(index, collapse = "]]$assignments[["), "]] <- NULL")
        eval(parse(text = index_cmd))
    }
    return(policy_categories)
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
    original_name <- flat_policy$categories[[getIndex(flat_policy, original_name)]]$category
    index <- find_indices(policy_categories, original_name)
    index <- paste0("policy_categories[[",paste(index, collapse = "]]$assignments[["), "]]")
    eval(parse(text = paste("category", "<-", index))) #category now stores original version of category
    new_category <- createCategory(name, input, assigns_table) #needs to be updated
    #at this point, new_category has all updated qualities except nested subcats, if applicable
    if (length(input$assignments) != 0){
        #if assignments are only subcats
        if(sum(input$assignments %in% assigns_table$assignment) == 0){
            existing_subcats <- purrr::map(category$assignments, "category")
            assignments <- list()
            for (assign in input$assignments){
                if (assign %in% existing_subcats){
                    i <- which(existing_subcats == assign)
                    print(category$assignments[[i]])
                    assignments <- append(assignments, list(category$assignments[[i]]))
                } else {
                    sub_cat <- createEmptyCategory(assign)
                    assignments <- append(assignments, list(sub_cat))
                }
            }
            new_category$assignments <- assignments
        }
    }
    eval(parse(text = paste(index, "<-", "new_category")))
    return (policy_categories)
}

getIndex <- function(flat_policy, name){
    names <- purrr::map(flat_policy$categories, "category") |> unlist() |>
        gsub(pattern = "[^a-zA-Z0-9]+", replacement = "")
    name <- gsub(pattern = "[^a-zA-Z0-9]+", replacement = "", name)
    which(names == name)
}
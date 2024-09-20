
#returns list of assignments with no assigned category
getUnassigned <- function(assign_table){
    assigns <- assign_table[["assignment"]][assign_table$category == "Unassigned"]
    if (length(assigns) != 0){
        return (assigns) 
    }
    return ()
}

updateAssignsTable <- function(assign_table, flat_policy){
    assign_table[["category"]] = "Unassigned"
    for (cat in flat_policy$categories){
        assign_table[["category"]][assign_table$assignment %in% cat$assignments] = cat$category
    }
    
    return (assign_table)
}


### This helper function assigns level of depth/nesting of each category for the purpose of nesting in the UI
assignLevelsToCategories <- function(flat_categories) {
    # Create a mapping from category names to their children.
    children_map <- lapply(flat_categories, function(cat) cat$assignments)
    names(children_map) <- sapply(flat_categories, `[[`, "category")
    
    # Initialize levels with 1 for top-level categories.
    levels <- rep(1, length(flat_categories))
    names(levels) <- names(children_map)
    
    # Function to determine the level of a category.
    calculate_level <- function(category_name, map) {
        for (parent_name in names(map)) {
            if (category_name %in% map[[parent_name]]) {
                return(1 + calculate_level(parent_name, map))
            }
        }
        return(1)
    }
    
    # Assign levels to each category.
    for (category_name in names(children_map)) {
        levels[category_name] <- calculate_level(category_name, children_map)
    }
    levels
}


createNestedCards <- function(flat_categories, category_levels) {
    ui_elements <- list()
    labels <- list(edit = list(), delete = list())
    
    # Helper function to create a box for a category
    createCategoryBox <- function(category, level, assignments_list) {
        label <- gsub(pattern = "[^a-zA-Z0-9]+", replacement = "", category$category)
        
        # Check if the category is "Overall Grade" > If true, make custom BOX
        if (category$category == "Overall Grade") {
            
            title <- div(
                class = "category-title", 
                category$category,
                actionButton(paste0('edit', label), label = NULL, icon = icon("pen-to-square"), style = "background-color: transparent;"),
            )
            
            content <- div(
                p("This is your overarching category. Start creating your syllabus here."),
                strong("Weight: "), category$weights %||% "100%", br(),
                strong("Categories: "), if (assignments_list == "No assignments") "No categories yet" else assignments_list
            )
            
            #If category is NOT "Overall Grade" then, no restrictions
        } else {
            
        title <- div(class = "category-title", 
                     category$category,
                     actionButton(paste0('delete', label), label = NULL, icon = icon("trash-can"), style = "background-color: transparent; margin-right: 10px;"),
                     actionButton(paste0('edit', label), label = NULL, icon = icon("pen-to-square"), style = "background-color: transparent;"),
                     
                     )
        content <- div(
            strong("Weight: "), category$weights %||% "Not set", br(),
            strong("Aggregation: "), getAggregationName(category_aggregation=category$aggregation), br(),
            strong("Number of Drops: "), " " %||% "Not set", br(),
            strong("Lateness Intervals:"), unname(sapply(list(category$lateness), format_policy, simplify = FALSE)) %||% "Not set", br(),
            strong("Assignments: "), assignments_list
        )
        }
        style <- if (level > 1) "margin-left: 20px;" else ""
        box(title = title, status = "primary", collapsible = TRUE, collapsed = !(level %in% c(1, 2)),  width = 12, div(style = style, content))
        
    }

    # Function to recursively create cards for nested categories
    createNestedUI <- function(category_name, level) {
        children <- lapply(flat_categories, function(cat) {
            if (cat$category %in% category_name$assignments) {
                createNestedUI(cat, level + 1)
            }
        })
        children_ui <- do.call(tagList, children)
        assignments_list <- if (is.null(category_name$assignments)) {
            "No assignments"
        } else {
            paste(category_name$assignments, collapse = ", ")
        }
        createCategoryBox(category_name, level, assignments_list) %>% tagAppendChild(children_ui)
    }

    # Loop over flat_categories and create the UI elements
    for (cat in flat_categories) {
        level <- category_levels[cat$category]
        label <- gsub(pattern = "[^a-zA-Z0-9]+", replacement = "", cat$category)
        # Create IDs for buttons
        edit_id <- paste0('edit', label)
        delete_id <- paste0('delete', label)
        # Store IDs
        labels$edit[[cat$category]] <- edit_id
        labels$delete[[cat$category]] <- delete_id
        if (level == 1) {  # Check if it is a top-level category
            ui_elements[[cat$category]] <- createNestedUI(cat, level)
        }
    }

    # Combine all the elements into a single UI element
    ui <- do.call(tagList, ui_elements)
    return(list(ui = ui, labels = labels))
}

getAggregationName <- function(category_aggregation) {
    switch(category_aggregation,
           'weighted_mean'= "Weighted Mean",
           'equally_weighted' = 'Equally Weighted',
           'weighted_by_points' = 'Weighted By Points',
           'max_score' = 'Max Score',
           'min_score' = 'Min Score',
           'none' = 'None')
}

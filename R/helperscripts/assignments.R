
#returns list of assignments with no assigned category
getUnassigned <- function(assign_table){
    assigns <- assign_table[["assignment"]][assign_table$category == "Unassigned"]
    if (length(assigns) != 0){
        return (assigns) 
    }
    return ("No more new assignments")
}

updateAssignsTable <- function(assign_table, flat_policy){
    assign_table[["category"]] = "Unassigned"
    for (cat in flat_policy$categories){
        assign_table[["category"]][assign_table$assignment %in% cat$assignments] = cat$category
    }
    
    return (assign_table)
}

# 
# 
# createCards <- function(categories, level = 1) {
#     ui_elements <- lapply(seq_along(categories), function(i) {
#         category <- categories[[i]]
#         name <- category$category
#         label <- gsub(pattern = "[^a-zA-Z0-9]+", replacement = "", name)
# 
#         # Prepare the assignments text if available
#         assignments_text <- ""
#         if (!is.null(category$assignments) && length(category$assignments) > 0) {
#             assignments_text <- sapply(category$assignments, function(a) a$category, USE.NAMES = FALSE)
#             assignments_text <- paste(assignments_text, collapse = ", ")
#         } else {
#             assignments_text <- "No assignments"
#         }
# 
#         # Create the box content
#         box_content <- div(
#             h5("Weight: ", category$weight),
#             h5("Aggregation: ", category$aggregation),
#             h5("Assignments: ", assignments_text)
#         )
# 
#         # Create the subcategories UI if they exist
#         subcategories_ui <- NULL
#         if (!is.null(category$assignments) && length(category$assignments) > 0) {
#             subcategories_ui <- lapply(category$assignments, function(subcat) {
#                 if (!is.null(subcat$assignments)) {
#                     print(subcat$assignments)
#                     createCards(subcat$assignments, level + 1)
#                 }
#             })
#             subcategories_ui <- do.call(tagList, subcategories_ui)
#         }
# 
#         # Create the box
#         box(
#             id = paste0("cat", label),
#             title = div(class = "category-title", name),
#             status = "primary",
#             actionButton(paste0('delete', label), label = NULL, icon = icon("trash-can"), style = "background-color: transparent; margin-right: 10px;"),
#             actionButton(paste0('edit', label), label = NULL, icon = icon("pen-to-square"), style = "background-color: transparent;"),
#             width = "100%",
#             collapsible = TRUE,
#             collapsed = (level != 1), # Collapse all but the first level
#             box_content,
#             subcategories_ui # Add the subcategories if any
#         )
#     })
# 
#     # Combine all elements into a single UI element
#     do.call(tagList, ui_elements)
# }


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
    print(levels)
    levels
}





createNestedCards <- function(flat_categories, category_levels) {
    ui_elements <- list()

    # Helper function to create a box for a category
    createCategoryBox <- function(category, level, assignments_list) {
        title <- div(class = "category-title", category$category)
        content <- div(
            strong("Weight: "), category$weight %||% "Not set", br(),
            strong("Aggregation: "), category$aggregation, br(),
            strong("Number of Drops: "), " " %||% "Not set", br(),
            strong("Lateness Intervals:"), " " %||% "Not set", br(),
            strong("Assignments: "), assignments_list
        )
        style <- if (level > 1) "margin-left: 20px;" else ""
        box(title = title, status = "primary", collapsible = TRUE, collapsed = (level != 1),  width = 12, div(style = style, content))
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
        if (level == 1) {  # Check if it is a top-level category
            ui_elements[[cat$category]] <- createNestedUI(cat, level)
        }
    }

    # Combine all the elements into a single UI element
    do.call(tagList, ui_elements)
}



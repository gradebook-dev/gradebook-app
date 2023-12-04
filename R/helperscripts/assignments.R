getUnassigned <- function(assign_table){
    left <-assign_table %>% 
        filter(category == "Unassigned")
    if (nrow(left) != 0){
        return (left$colnames) 
    }
    return ("No more new assignments")
}

#updates assignments in assignment table when they are assigned a category
updateAssigns <- function(assigns_table, assignments, original_cat, new_cat){
    assigns_table <- resetAssigns(assigns_table, original_cat)
    if (!is.null(assignments)){
        selected <- data.frame(colnames = assignments)
        selected <- semi_join(assigns_table, selected, "colnames") %>% mutate(category = new_cat)
        assigns_table <- rbind(selected, anti_join(assigns_table, selected, "colnames"))
    }
    return (assigns_table)
}

#resets all assignments of a category to "Unassigned"
resetAssigns <- function(assigns_table, original_category){
    if (!is.null(original_category)){
    selected <- assigns_table %>%
        filter(category == original_category) %>%
        mutate(category = "Unassigned")
    assigns_table <- rbind(selected, anti_join(assigns_table, selected, "colnames"))
    }
    return (assigns_table)
}
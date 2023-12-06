#returns list of assignments with no assigned category
getUnassigned <- function(assign_table){
    left <-assign_table %>% 
        filter(category == "Unassigned")
    if (nrow(left) != 0){
        return (left$assignment) 
    }
    return ("No more new assignments")
}

#updates assignments in assignment table when they are assigned a category
updateAssigns <- function(assigns_table, assignments, original_cat, new_cat){
   
}

#resets all assignments of a category to "Unassigned"
resetAssigns <- function(assigns_table, original_category){
    
}
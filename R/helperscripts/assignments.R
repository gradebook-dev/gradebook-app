
#returns list of assignments with no assigned category
getUnassigned <- function(assign_table){
   assigns <- assign_table[["assignment"]][assign_table$category == "Unassigned"]
    if (length(assigns) != 0){
        return (assigns) 
    }
    return ("No more new assignments")
}

updateAssignsTable <- function(assign_table, flat_policy){
    for (cat in flat_policy$categories){
        assign_table[["category"]][assign_table$assignment %in% cat$assignments] = cat$category
    }
    
    return (assign_table)
}
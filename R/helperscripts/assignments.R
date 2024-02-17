
#returns list of assignments with no assigned category
getUnassigned <- function(assign_table){
   assigns <- assign_table[["assignment"]][assign_table$category == "Unassigned"]
    if (length(assigns) != 0){
        return (assigns) 
    }
    return ("No more new assignments")
}

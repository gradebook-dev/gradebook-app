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
updateAssignsTable <- function(assign_table, input, flat_policy, editing_nr){
    #reset assignments assigned to existing category
    if (editing_nr != 0){ 
        i <- getIndex(flat_policy, editing_nr)
        original_cat <- flat_policy[[i]]$category
        assign_table$assignment[assign_table$category == original_cat] <- "Unassigned"
        
    }
    #update assignments with category
    if (!is.null(input$assignments)){
        assigns <- input$assignments[input$assignments %in% assign_table$assignment]
        subcats <- input$assignments[!(input$assignments %in% assign_table$assignment)]
        assign_table$category[assign_table$assignment %in% input$assignments] <- input$name
        subcats <- data.frame(assignment = subcats) |>
            mutate(category = input$name)
        assign_table <- rbind(assign_table, subcats)
    }
    assign_table
}

#resets all assignments of a category to "Unassigned"
resetAssigns <- function(assigns_table, original_category){
    
}
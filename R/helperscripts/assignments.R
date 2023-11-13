#create assignment table from input data
createAssignTable <- function(data, subcat_table) {
    gs_cols <- names(data)
    
    #adding the new assignemtns from subcategories:
    subcat_values <- subcat_table$subcat_colname
    combined_cols <- c(gs_cols, subcat_values)
    
    #TEMPORARY SOLUTION FOR DIFFERING COLNAMES
    
    #data 88E specific
    if (gs_cols[1] == "Name"){
        gs_cols[1] <- "Names"
    }
    
    #stat 20 specific - TBD
    
    assignments <- tibble(colnames = combined_cols) %>%
        #General regex to rename assignments and add a column "category" in table
        mutate(new_colnames = str_replace_all(tolower(colnames), "[\\s:]+", "_"),
               #     category = substr(str_replace_all(tolower(colnames), "[\\s:]+", "_"), 1, regexpr("_", str_replace_all(tolower(colnames), "[\\s:-]+", "_")) - 1),
               type = if_else(!str_detect(new_colnames, "name|sections|max|time|late|email|sid"), "_-_raw_points", "" ),
               new_colnames = paste0(new_colnames, type))%>% # concatenate gs_col and type
        select(new_colnames, colnames) %>%
        mutate(category = "Unassigned")
    
    # Replace all "," with ":" in the colnames to avoid the issue with selecting these assignments in creating categories.
    assignments$colnames <- str_replace_all(assignments$colnames, ",", ":")
    assignments$new_colnames <- str_replace_all(assignments$new_colnames, ",", ":")
    
    return(assignments)
}

getUnassigned <- function(assign_table){
    print(assign_table)
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
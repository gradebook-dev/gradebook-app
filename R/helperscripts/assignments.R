#create assignment table from input data
createAssignTable <- function(data) {
    gs_cols <- names(data)
    
    #TEMPORARY SOLUTION FOR DIFFERING COLNAMES
    
    #data 88E specific
    if (gs_cols[1] == "Name"){
        gs_cols[1] <- "Names"
    }
    
    #stat 20 specific - TBD
    
    assignments <- tibble(colnames = gs_cols) %>%
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
    left <-assign_table %>% 
        filter(category == "Unassigned")
    if (nrow(left) != 0){
        return (left$colnames) 
    }
    return ("No more new assignments")
}

#updates assignments in assignment table when they are assigned a category
updateAssigns <- function(assignments, assign, cat_name){
    if (is.null(assign)){
        assignments <- assignments %>% mutate(category = cat_name)
    } else {
        selected <- data.frame(colnames = assign)
        selected <- semi_join(assignments, selected, "colnames") %>% mutate(category = cat_name)
        assignments <- rbind(selected, anti_join(assignments, selected, "colnames"))
    }
    return (assignments)
}

#resets all assignments of a category to "Unassigned"
resetAssigns <- function(assignments, original_category){
    selected <- assignments %>%
        filter(category %in% original_category) %>%
        mutate(category = "Unassigned")
    assignments <- rbind(selected, anti_join(assignments, selected, "colnames"))
    return (assignments)
}
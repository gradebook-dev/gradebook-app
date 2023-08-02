process_sids <- function(new_data) {
    df_filtered <- new_data %>% filter(!is.na(new_data$sid))
    
    duplicated_rows <- df_filtered %>%
        group_by(sid) %>%
        filter(n() > 1) %>%
        ungroup()
    
    duplicates_df <- new_data %>%
        filter(is.na(sid) | (sid %in% duplicated_rows$sid)) %>%
        arrange(sid)
    
    #df with only unique sid
    unique_sids <- new_data[!(new_data$sid %in% duplicates_df$sid), ]
    
    #combined df with unique and na in sid
    unique_and_na_sids <- rbind(unique_sids, filter(duplicates_df, is.na(sid)))
    
    #df with duplicate sid so we can do merging using only this small df
    duplicate_sids_without_na <- filter(duplicates_df, !is.na(sid))
    
    if (nrow(duplicate_sids_without_na) > 0) {
        data_uniquesids <- duplicate_sids_without_na %>%
            group_by(sid) %>%
            summarize(across(
                everything(),
                ~ if (inherits(., "Period")) {
                    last(na.omit(.))
                } else if (is.numeric(.)) {
                    max(., na.rm = TRUE)
                } else {
                    if (all(is.na(.))) {
                        NA
                    } else {
                        last(na.omit(.))
                    }
                }
            )) %>%
            ungroup()
        
        #combine dataframes into 1 correct dataframe of students
        result <- rbind(unique_and_na_sids, data_uniquesids)
    } else {
        result <- new_data
    }
    
    return(list(unique_sids = result, duplicates = duplicates_df))
}


pivot <- function(new_data, assignments_dataframe, cat_table){
    id_cols <- c("names", "sections","email", "sid")
    
    new_data <- as.data.frame(new_data)
    
    sxa <- new_data %>%
        pivot_longer(!all_of(id_cols), # change the unit of obs to student x assignment
                     names_to = c("assignments", ".value"),
                     names_sep = "_-_") %>%
        replace_na(list(raw_points = 0))
    
    if (!is.null(assignments_dataframe)) {
        assignments_dataframe$new_colnames <- str_replace_all(assignments_dataframe$new_colnames, "_-_raw_points", "")
    }
    
    add_categories_to_pivot <- sxa %>%
        left_join(assignments_dataframe %>% select(new_colnames, colnames, category), by = c("assignments" = "new_colnames"))
    colnames(add_categories_to_pivot)[colnames(add_categories_to_pivot) == "lateness_(h_m_s)"] ="lateness_min"
    
    #cat_table is the list with categories (policy$categories) which is converted to a data frame categories_df in the server.
    x <- length(cat_table)
    if (x > 0){
        #remove assigns column from cat_table
        cat_table <- select(cat_table, -assigns)
        add_categories_to_pivot <- add_categories_to_pivot %>%
            left_join(cat_table, by = c("category" = "name"))
    }
    return(add_categories_to_pivot)
}


# this allows lubridate values to be saved in the dataframe
convert_to_min <- function(hms){
    save <- lubridate::hms(hms)
    save <- period_to_seconds(save)
    save <- save/60
    return (save)
}
# The idea for all_grades is the following:
# Note: policy$categories and pivot are already merged at this point
#
# CategoryGrades():
#       1) filter out unassigned assignments
#       2) use (number of assignments - number of drops) to determine how many relevant
#           assignments in each category --> will be used when weighting assignments equally
#       3) add a column for calculating score after lateness if applied
#           convert late_time1, late_time2 to minutes
#
# ### DATA 100 SPECIFIC lateness !!
#
#
#       4) drops lowest n score after lateness
#       5) sum up all max points (discluding dropped assignment)
#       count max points per category
#       6) Calculate grade based on agggregation method
#       calculating score based on weights EQUALLY WEIGHTED
#       these need be to averaged
#       7)  #calculating score based on weights 
#       8)  #merge dataframes - equally weighted and by points


# GradesPerCategory:
#       9) sum the grades after weight for each assignment and divide by the % weight
#        to get the total score per category for each student
#       10) pivot wider to make 1 row per student with their respective scores per category              
#       11) adding final grade score column
#       12 adding letter grade based on grade bins
#


CategoryGrades <- function(pivotdf){
    
    #1 filter out unassigned assignments
    df_assigned_assignments <- pivotdf %>%
        filter(category != "Unassigned")

    #2 use (number of assignments - number of drops) to determine how many relevant
    #  assignments in each category --> will be used when weighting assignments equally
    df_assigned_assignments <- df_assigned_assignments %>%
        mutate(relevant_assigns = as.numeric(num_assigns) - as.numeric(drops))


    #3 add a column for calculating score after lateness if applied
    # convert late_time1, late_time2 to minutes
    df_assigned_assignments <- df_assigned_assignments %>%
        mutate(
            late_time1_min = round(convert_to_min(late_time1), 3),
            late_time2_min = round(convert_to_min(late_time2), 3),
        )

    #until
    df_with_lateness <- df_assigned_assignments %>%
        mutate(points_after_lateness = case_when(
            #late1
            #lateness_min > 0 & lateness_min <= late_time1_min ~ raw_points*as.numeric(late_scale1),
            #data 100 specific
            lateness_min <= late_time1_min ~ raw_points*as.numeric(late_scale1),
            #between late1 and late2
            lateness_min > late_time1_min & lateness_min <= late_time2_min ~ raw_points*as.numeric(late_scale2),
            #past late2
            lateness_min > late_time2_min ~ raw_points*0,
            #not late
            TRUE ~ raw_points
        )) %>%
         mutate(score_after_lateness = points_after_lateness/max_points)
    
    # #after
    # df_with_lateness <- df_assigned_assignments %>%
    #     mutate(points_after_lateness = case_when(
    #         #if lateness is after late1 but before late2, scale by late1_scale
    #         lateness_min > late_time1_min & lateness_min <= late_time2_min ~ raw_points*as.numeric(late_scale1),
    #         #if lateness is after late2, scale by late2_scale
    #         lateness_min > late_time2_min ~ raw_points*as.numeric(late_scale2),
    #         #default/ before late1, scale by 1
    #         TRUE ~ raw_points
    #     )) %>%
    #     mutate(score_after_lateness = points_after_lateness/max_points)
    
    #4 drops lowest n score after lateness
    num_drops_in_class <- df_with_lateness %>%
        summarize(sum_drops = sum(as.numeric(drops)))
        df_with_drops <- df_with_lateness %>%#default if no drops in class
            mutate(dropped = FALSE)
    if (num_drops_in_class > 0){
        kept_assignments <- df_with_lateness %>%
            group_by(sid, category) %>%
            arrange(score_after_lateness) %>% #arrange in ascending order based on group_by
            slice( (as.numeric(drops) + 1) :n() ) %>% #drop the number of drops and keep the rest assignments
            mutate(dropped = FALSE)
        
        dropped_assignments <- df_with_lateness %>%
            filter(as.numeric(drops) > 0) %>%
            group_by(sid, category) %>%
            arrange(score_after_lateness) %>% #arrange in ascending order based on group_by
            slice(1: as.numeric(drops)) %>% #drop the number of drops and keep the rest assignments
            mutate(dropped = TRUE)
        
        df_with_drops <- rbind(kept_assignments, dropped_assignments) %>%
            arrange(sid)   
    }
                
    #5 sum up all max points (discluding dropped assignment)
     #count max points per category
     count_max_points_per_category <- df_with_drops %>%
         filter(dropped == FALSE) %>%
         group_by(sid,category) %>%
         summarise(total_max_points_per_cat = sum(as.numeric(max_points)))


    #join total count of points with main pivot table
    df_with_max_points <- df_with_drops%>%
    left_join(count_max_points_per_category, by = c("sid", "category"))
    
    #6 Calculate grade based on agggregation method
    #calculating score based on weights EQUALLY WEIGHTED
    #these need be to averaged
    equally_weighted <- df_with_max_points%>%
        filter(aggregation == "Equally Weighted")%>%
        #this should yield the raw final percentage earned per assignment
    
        mutate(grade_after_weight = score_after_lateness*((as.numeric(weight)/100)/relevant_assigns))
    
    #calculating score based on weights WEIGHTED BY POINTS
    #these need to be summed
     weighted_by_points <- df_with_max_points%>%
        filter(aggregation == "Weighted by Points")%>%
        #this should yield the raw final percentage earned per assignment
        mutate(grade_after_weight = ((points_after_lateness/total_max_points_per_cat)*(as.numeric(weight)/100)))

    #merge dataframes - equally weighted and by points
     combined_data <- bind_rows(equally_weighted, weighted_by_points)

    # return(df_with_lateness_and_max_points_per_cat)
    return (combined_data)
}


GradesPerCategory <- function(allgradestable, cutoff){

    ###CLOBBER:
    #8 3/4
    clobber_df <- allgradestable%>%
        #select the 2 columns and get the distinct category + clobber
        select(category,clobber)%>%
        distinct(category, clobber)
    print(clobber_df)
 
    #9
    grades_per_category <- allgradestable %>%
        #keep all NOT dropped assignments
        filter(dropped == FALSE) %>%
        #grouping by student id and category
        group_by(sid, category) %>%
        summarise(
            #keep the name of the student as well
            names = first(names),
            #sum the grades after weight for each assignment and divide by the % weight
            #to get the total score per category for each student
            percent_grade_per_category = (sum(grade_after_weight) / (first(as.numeric(weight))/100)),
            .groups = 'drop'
        )
    
    #10 pivot wider to make 1 row per student with their respective scores per category
    grades_per_category_wider <- grades_per_category %>%
        pivot_wider(names_from = category, values_from = percent_grade_per_category)
    
    print(grades_per_category_wider)
    #10 3/4
    grades_per_category_clobbered <- grades_per_category_wider%>%
        #dropping NA's but this should not be needed for real data. 
        #very important to remove this when using real data or maybe count any NA's below 
        #so we do not remove students accidentally
        drop_na()
    
    #checking for NAs
    # has_na <- sum(is.na(grades_per_category_clobbered))
    # print(has_na)
    
    
    #I am unsure about this forloop - i just did not seem to find any other way
    
    # Loop through each row of the clobber_df data frame: this will loop through each category
    for (i in 1:nrow(clobber_df)) {
        category <- clobber_df$category[i]
        clobbered <- clobber_df$clobber[i]
        #true only if there is a clobber (not "None")
        if (clobbered != "None") {
            # Loop through each student in the df
            for (j in 1:nrow(grades_per_category_clobbered)) {
                # Check if the score in the clobbered category is greater than the score in the initial category
                if (grades_per_category_clobbered[j, clobbered] > grades_per_category_clobbered[j, category]) {
                    # Update the score in the initial category if clobbered category contains larger value
                    grades_per_category_clobbered[j, category] <- grades_per_category_clobbered[j, clobbered]
                }
            }
        }
    }
    print(grades_per_category_clobbered)
   
    #11 adding final grade score column
    grades_per_category_clobbered$course_grade <- apply(grades_per_category_clobbered[, -c(1, 2)], 1, mean, na.rm = TRUE)*100
    
    #12 adding letter grade based on grade bins
    grades_per_category_clobbered <- grades_per_category_clobbered %>%
        mutate(course_letter_grade = case_when(
            course_grade >= cutoff$A ~ "A",
            course_grade < cutoff$A & course_grade >= cutoff$B ~ "B",
            course_grade < cutoff$B & course_grade >= cutoff$C ~ "C",
            course_grade < cutoff$C & course_grade >= cutoff$D ~ "D",
            course_grade < cutoff$D ~ "F",
            TRUE ~ "NA"
        ))
    return(grades_per_category_clobbered)
}

#updating the bins after changing inputs in the UI
updateBins <- function(cutoff, input_A, input_B, input_C, input_D, input_F){
    cutoff$A <- as.numeric(input_A)
    cutoff$B <- as.numeric(input_B)
    cutoff$C <- as.numeric(input_C)
    cutoff$D <- as.numeric(input_D)
    cutoff$F <- as.numeric(input_F)
    return (cutoff)
}

getStudentConcerns <- function(grades_table){
    save <- grades_table %>% filter(course_letter_grade == "F") %>%
        mutate(Concerns = paste0(names, " (score: ", course_grade, ")"))
    if (!is.null(save)){
        return (save$Concerns)
    }
    return ("No student concerns here")
}
getGradeStats <- function(grades_table){
    mean <- paste0("Mean: ", round(mean(as.numeric(grades_table$course_grade)),2))
    median <- paste0("Median: ", median(as.numeric(grades_table$course_grade)))
    sd <- paste0("Standard Deviation: ", round(sd(as.numeric(grades_table$course_grade)),2))
    stats <- c(mean,median, sd)
    # l <- ncol(grades_table)-2
    # for (x in 2:l){ #iterates through all category grades
    #     name <- colnames(grades_table)[x]
    #     values <- as.numeric(grades_table[x])
    #     mean <- mean(values, na.rm = TRUE)
    #     stats <- append(stats, paste0("Category ", tools::toTitleCase(name), " Mean : ", round(mean, 2)))
    # 
    # }
    return (stats)
}

# The idea for all_grades is the following:
# Note: policy$categories and pivot are already merged at this point
#
# 1) add columns for each grading policy from syllabus
# 2) add columns: i) counts of number of assignments per category
#                 ii)calculating score after lateness if applied
#                iii)add count of total max points per a whole category (i.e. 2labs 10points each = 20 total points)
#
#
# Create another table GradesPerCategory:
# 3) start merging: 
#                   drop lowest graded assignments per drop policy
#                   group by category and student (still have several rows per student 
#                   but just 1 row per category)
#                   determine percentage grade per category


AllGradesTable <- function(pivotdf, policy_categories_list, assigns_table){
    
    #1 filter out unnasigned assignments
    df_assigned_assignments <- pivotdf %>%
        filter(category != "Unassigned")

    #2 count assignments per category for calculating weights
    category_counts <- assigns_table %>%
        group_by(category) %>%
        summarise(count = n())%>%
        ungroup()%>%
        filter(category != "Unassigned")
    
    print(category_counts)
        
    # Add the counts back to the main dataframe
    df_assigned_assignments <- df_assigned_assignments %>%
        left_join(category_counts, by = "category")


    #3 add a column for calculating score after lateness if applied
    
    # convert late_time1, late_time2 to minutes
    df_assigned_assignments <- df_assigned_assignments %>%
        mutate(
            late_time1_min = hour(hms(late_time1))*60 + minute(hms(late_time1)),
            late_time2_min = hour(hms(late_time2))*60 + minute(hms(late_time2))
        )
    print(df_assigned_assignments)
    
    df_with_lateness <- df_assigned_assignments %>%
        mutate(score_after_lateness = case_when(
            #late1
            lateness_min > 0 & lateness_min <= late_time1_min ~ raw_points*as.numeric(late_scale1),
            #between late1 and late2
            lateness_min > late_time1_min & lateness_min <= late_time2_min ~ raw_points*as.numeric(late_scale2),
            #past late2
            lateness_min > late_time2_min ~ raw_points*0,
            #not late
            TRUE ~ raw_points
        ))
    
     #count max points per category
     count_max_points_per_category <- df_assigned_assignments%>%
       group_by(sid,category) %>%
       summarise(total_max_points_per_cat = sum(as.numeric(max_points)))

    print(count_max_points_per_category)

    #join total count of points with main pivot table
    df_with_lateness_and_max_points_per_cat <- df_with_lateness%>%
    left_join(count_max_points_per_category, by = c("sid", "category"))

    
    #calculating score based on weights EQUALLY WEIGHTED
    #these need be to averaged
    # equally_weighted <- pivotdf_assigned_assignments%>%
    #     filter(Grading_Policy == "Equally Weighted")%>%
    #     #this should yield the raw final percentage earned per assignment
    #     mutate(grade_after_weight = round(((score_after_lateness/max_points)/count_assignments), 2))

    #calculating score based on weights WEIGHTED BY POINTS
    #these need to be summed
    # weighted_by_points <- pivotdf_assigned_assignments%>%
    #     filter(Grading_Policy == "Weighted by Points")%>%
    #     #this should yield the raw final percentage earned per assignment
    #     mutate(grade_after_weight = round(((score_after_lateness/count_assignments) * as.numeric(Weights)), 2))

    #merge dataframes - equally weighted and by points
    # combined_data <- bind_rows(equally_weighted, weighted_by_points)

    return(df_with_lateness_and_max_points_per_cat)
}

# 
# GradesPerCategory <- function(allgradestable){
#     grades_per_category <- allgradestable %>%
#         group_by(sid, category) %>%
#         arrange(raw_pts_after_lateness) %>% #arrange in ascending order based on group_by
#         slice(((as.numeric(Drops) + 1):n())) %>% #drop the number of drops and keep the rest assignments
#         #   summarise(percentage_grade = round((sum(grade_after_weight) / sum(max_points)), 2))%>%
#         mutate(percentage_grade = round((sum(grade_after_weight) / sum(max_points)), 2)) %>%
#         select(names, sid, category, Grading_Policy, percentage_grade)
#     
#     # equally_weighted <- grades_per_category%>%
#     #   group_by(sid, category)
#     
#     
#     return(grades_per_category)
# }
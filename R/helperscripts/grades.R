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


CategoryGrades <- function(pivotdf){
    
    #1 filter out unassigned assignments
    df_assigned_assignments <- pivotdf %>%
        filter(category != "Unassigned")

    #2 use (number of assignments - number of drops) to determine how many relevant
    #  assignments in each category --> will be used when weighting assignments equally
    num_relevant_assigns <- df_assigned_assignments %>%
        mutate(relevant_assigns = as.numeric(num_assigns) - as.numeric(drops))


    #3 add a column for calculating score after lateness if applied
    # convert late_time1, late_time2 to minutes
    df_assigned_assignments <- df_assigned_assignments %>%
        mutate(
            late_time1_min = convert_to_min(late_time1),
            late_time2_min = convert_to_min(late_time2),
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
        kept_assignments <- df_with_lateness %>%
            group_by(sid, category) %>%
            arrange(score_after_lateness) %>% #arrange in ascending order based on group_by
            slice( (as.numeric(drops) + 1) :n() ) %>% #drop the number of drops and keep the rest assignments
            mutate(dropped = FALSE)
        dropped_assignments <- df_with_lateness %>%
            filter(drops > 0) %>%
            group_by(sid, category) %>%
            arrange(score_after_lateness) %>% #arrange in ascending order based on group_by
            slice(1: as.numeric(drops)) %>% #drop the number of drops and keep the rest assignments
            mutate(dropped = TRUE)
            
        df_with_drops <- rbind(kept_assignments, dropped_assignments) %>%
            arrange(sid)
                
    #5 sum up all max points (discluding dropped assignment)
     #count max points per category
     count_max_points_per_category <- df_with_drops %>%
         filter(dropped == FALSE) %>%
         group_by(sid,category) %>%
         summarise(total_max_points_per_cat = sum(as.numeric(max_points)))


    #join total count of points with main pivot table
    df_with_max_points <- df_with_drops%>%
    left_join(count_max_points_per_category, by = c("sid", "category"))
    
    return(df_with_max_points)
    
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

    # return(df_with_lateness_and_max_points_per_cat)
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
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

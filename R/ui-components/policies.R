Policies <- tabItem(tabName = "policies",
                 h2("Create Your Policy File"),
                 ### COURSE NAME ###
                 fluidRow(
                     tagList(
                         div(style = "padding: 0px 20px 20px 20px;  background-color: white;",
                             div(
                                 
                                 style = "border: 1px solid #000; padding: 10px; border-radius: 5px; margin-top: 20px;",
                                 tags$div(
                                     style = "display: flex; justify-content: left; align-items: center;",
                                     tags$div(
                                         textOutput("course_name_display"),
                                         style = "font-size: 20px; display: inline-block; margin-right: 10px;"
                                     ),
                                     actionButton("edit_policy_name", label = NULL, icon = icon("pen-to-square"), style = "background-color: transparent; "),
                                     actionGroupButtons(inputIds = c("save_json"), labels = c("Save this Course")),
                                     style = "border: none; background: transparent;"
                                     
                                 ),
                                 tags$div(
                                     textOutput("course_description_display"),
                                     style = "margin-top: 20px;"
                                 )
                             )
                         )
                     )
                 )
)
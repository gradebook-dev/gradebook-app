Dashboard <- tabItem(tabName = "dashboard",
                        fluidRow(
                                     h2("Dashboard"),
                                    textInput("course_name", "Course Name", value = ""),
                                    downloadButton("download_policy")
                                    # uiOutput("dashboard")
                     )
)
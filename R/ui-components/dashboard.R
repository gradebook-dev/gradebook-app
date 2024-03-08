Dashboard <- tabItem(tabName = "dashboard",
                     fluidRow(
                         tagList(
                             div(style = "padding: 0px 20px 20px 20px;",
                                 h2("Dashboard"),
                                 plotOutput("dashboard")
                             ))
                     )
)
Dashboard <- tabItem(tabName = "dashboard",
                     fluidRow(
                         tagList(
                             div(class='dashboard',
                                 h2("Dashboard"),
                                 uiOutput("dashboard")
                             ))
                     )
)
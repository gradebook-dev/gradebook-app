library(shinydashboard)
library(shiny)
library(shinyWidgets)
#Create UI Comps
UICompDirectory <- "ui_components/"

source(paste0(UICompDirectory, "policies.R"), local = TRUE)
source(paste0(UICompDirectory, "dashboard.R"), local = TRUE)
source(paste0(UICompDirectory, "scratchpad.R"), local = TRUE)

shinyUI(
    dashboardPage(
        dashboardHeader(title = "Gradebook"),
        
        
        
        dashboardSidebar(
            ## Sidebar content
            sidebarMenu(
                menuItem("Policies", tabName = "policies", icon = icon("th")),
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Scratchpad", tabName = "scratchpad", icon = icon("pencil")),
                br(),
                hr(),
                div(class = "sidebar-text",
                    icon("upload", style = "margin-left: 20px;"),
                    "Upload Student Data"
                    
                ),
                fileInput("upload", label = NULL, accept = c(".csv")),
                hr(),
                div(class = "sidebar-text",
                    style = "margin-left: 20px;",
                    "Pick Course File:"
                ),
                div(style = "margin-top: -20px;",
                    selectizeInput("pick_policy", "",
                                   choices = '',
                                   multiple = FALSE,
                                   width = "100%"),
                ),
                div(style = "display: flex; align-items: center;",
                    actionButton("upload_json", "Upload Your Course"),
                    actionButton("delete_json", "", icon = icon("trash-can"))
                )
                
            )
            
            
        ),
        ## Body content
        dashboardBody(
            tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color:#50A5EA;
                                color: #1B2A3A;
                                font-family: "Georgia", Times, "Times New Roman";
                               # font-weight: bold;
                                font-size: 24px;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #50A5EA;
                               
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #50A5EA;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #2E3537;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #316590;
                                color: #FFFFFF;
                                }
                                
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #2E3537;
                                color: #FFFFFF;
                                }
                                
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #438BC5;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #438BC5;
                                }
                                /* toggle button  */
                                .skin-blue .main-header .navbar .sidebar-toggle{
                                color: #1B2A3A;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }
                                
                                '))),
            tabItems(
                Dashboard,
                Policies,
                scratchpad,
                tabItem(
                    tabName = "fileinput",
                    h2("Upload")
                )
            )
        ))
    
)
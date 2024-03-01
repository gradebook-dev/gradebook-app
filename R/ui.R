library(shinydashboard)

UICompDirectory <- "ui-components/"

source(paste0(UICompDirectory, "files.R"), local = TRUE)
source(paste0(UICompDirectory, "policies.R"), local = TRUE)

shinyUI(
    dashboardPage(
        dashboardHeader(title = "Gradebook"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Files", tabName = "files", icon = icon("pencil")),
                menuItem("Policies", tabName = "policies", icon = icon("pencil")),
                br(),
                hr(),
                div(class = "sidebar-text",
                    icon("upload", style = "margin-left: 20px;"),
                    "Upload Student Data"
                    
                ),
                fileInput("upload_gs", label = NULL, accept = c(".csv")),
                hr(),
                div(class = "sidebar-text",
                    style = "margin-left: 20px;",
                    "Upload Policy File:"
                ),
                fileInput("upload_policy", label = NULL, accept = c(".yml")),
                # div(style = "display: flex; margin-left:17px;",
                #     actionButton("upload_json", "Upload Policy File"),
                #     actionButton("delete_json", "", icon = icon("trash-can"))
                # ),
                # br(),
                hr(),
                div(style = "display: flex; align-items: center; margin-left: 15px;",
                    h5("Download Your Course Grades:")
                ),
                div(style = "display: flex; align-items: center; margin-left: 15px;",
                downloadButton("download_grades","Download Grades")
                ),
                hr(),
                div(style = "display: flex; align-items: center; margin-left: 15px;",
                    h5("Download Your Yaml Policy File:")
                ),
                div(style = "display: flex; align-items: center; margin-left: 15px;",
                    downloadButton("download_policy_file","Download Policy File")
                )
            )
        ),
        dashboardBody(
            tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color:#50A5EA;
                                color: #1B2A3A;
                               # font-family: "Georgia", Times, "Times New Roman";
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
                               
                                /* body */
                                .content-wrapper, .right-side{
                                background-color: #ffffff; 
                                }
                                 
                                '))),
            tabItems(
                Files,
                Policies
                )
        )
    )
)
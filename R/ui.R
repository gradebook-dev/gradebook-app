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
                #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Data Files", tabName = "scratchpad", icon = icon("pencil")),
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
                                 /* Help icon style */
                                .help-icon {
                                    position: fixed;
                                    bottom: 50px;
                                    right: 50px;
                                    cursor: pointer;
                                    font-size: 18px;
                                    z-index: 101;
                                   
                                }
                                
                                /* Tooltip box style */
                                .tooltip-box {
                                    display: none;
                                    position: fixed;
                                    bottom: 70px;
                                    right: 20px;
                                    background-color: #f9f9f9;
                                    border: 1px solid #ccc;
                                    padding: 10px;
                                    z-index: 100;
                                    width: 300px;
                                    border-radius: 5px;
                                    box-shadow: 0 2px 5px rgba(0,0,0,.2);
                                }
                                
                                /* Show tooltip box on hover */
                                .help-icon:hover + .tooltip-box {
                                    display: block;
                                }
                                
                                      '))),
            icon("question-circle", class = "help-icon"),
            tags$div(class = "tooltip-box", 
                     tags$div(
                         HTML('
            <h4>How to use Gradebook:</h4>
            <ul>
                <li>Load your gradescope file</li>
                <li>Create your syllabus policies in the "Assignment-view" tab (OR if you have a policy file already, load it from the sidebar to your left by "Pick Course File")</li>
                <li>Finish creating your policy, go to the dashboard to see grades scores</li>
            </ul>
        ')
                     )
            ),
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
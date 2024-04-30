library(shinydashboard)

UICompDirectory <- "ui-components/"
styleDirectory <- '../www/style.css'

source(paste0(UICompDirectory, "files.R"), local = TRUE)
source(paste0(UICompDirectory, "policies.R"), local = TRUE)
source(paste0(UICompDirectory, "dashboard.R"), local = TRUE)

shinyUI(
    dashboardPage(
        dashboardHeader(title = "Gradebook"),
        dashboardSidebar(
            sidebarMenu(
                tags$head(
                    tags$link(rel = 'stylesheet', type = 'text/css', href = styleDirectory)
                ),
                menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line", class = "fixed-width-icon")),
                menuItem("Policies", tabName = "policies", icon = icon("file-pen", class = "fixed-width-icon")),
                hr(),
                div(class = "sidebar-text",
                    icon("upload", class = 'sidebar-icon'),
                    "Upload Student Data"
                ),
                fileInput("upload_gs", label = NULL, accept = c(".csv")),
                hr(),
                div(class = "sidebar-text",
                    icon('upload', class = 'sidebar-icon'),
                    "Upload Policy File"
                ),
                fileInput("upload_policy", label = NULL, accept = c(".yml")),
                hr(),
              h5("Explore With Generic Data", class = 'sidebar-icon'),
              div(actionButton('demogs', "Use Demo Data and Policy")),
              hr(),
                div(class ='sidebar-text',
                    h5("Download Your Course Grades")
                ),
                div(class ='sidebar-text',
                div(downloadButton("download_grades","Download Grades")),
                ),
                hr(),
                div(class ='sidebar-text',
                    h5("Download Your Yaml Policy File")
                ),
                div(class = 'download-button',
                    downloadButton("download_policy_file","Download Policy File")
                )
            )
        ),
        dashboardBody(
            includeCSS(styleDirectory),
            tabItems(
                Policies,
                Dashboard
            )
        )
    )
)
library(shinydashboard)

UICompDirectory <- "ui-components/"
app_version <- "0.5.3"

source(paste0(UICompDirectory, "files.R"), local = TRUE)
source(paste0(UICompDirectory, "policies.R"), local = TRUE)
source(paste0(UICompDirectory, "dashboard.R"), local = TRUE)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Gradebook"),
    dashboardSidebar(
      sidebarMenu(
        tags$head(
          tags$style(HTML('
            .fixed-width-icon {
              width: 25px !important;
              text-align: left;
            }
            .version-tooltip {
              position: fixed;
              bottom: 10px;
              left: 10px;
              cursor: pointer;
            }
            .tooltiptext {
              visibility: hidden;
              font-size: .8em;
              background-color: #555;
              color: #fff;
              text-align: center;
              border-radius: 6px;
              padding: 5px;
              position: absolute;
              z-index: 1;
              left: 105%;
              top: 50%;
              margin-top: -15px;
              opacity: 0;
              transition: opacity 0.3s;
            }
            .version-tooltip:hover .tooltiptext {
              visibility: visible;
              opacity: 1;
            }
          '))
        ),
        # Existing menu items
        menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line", class = "fixed-width-icon")),
        menuItem("Policies", tabName = "policies", icon = icon("file-pen", class = "fixed-width-icon")),
        hr(),
        div(class = "sidebar-text", icon("upload", style = "margin-left: 20px;"), "Upload Student Data"),
        fileInput("upload_gs", label = NULL, accept = c(".csv")),
        hr(),
        div(class = "sidebar-text", icon('upload', style = "margin-left: 20px;"), "Upload Policy File"),
        fileInput("upload_policy", label = NULL, accept = c(".yml")),
        hr(),
        div(style = "display: flex; align-items: center; margin-left: 20px;", h5("Download Files")),
        div(style = "display: flex; align-items: center; margin-left: 15px;",
          div(style = "flex-grow: 1; margin-right: 10px;", downloadButton("download_grades", "Grades", style = "width: 100%;")),
          div(style = "flex-grow: 1; margin-right: 10px;", downloadButton("download_policy_file", "Policy File", style = "width: 100%;"))
        ),
        div(class = "version-tooltip",
          icon("question-circle"),
          div(class = "tooltiptext", paste("Gradebook Version:", app_version))
        )
      )
    ),
    dashboardBody(
      tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
          background-color:#50A5EA;
          color: #1B2A3A;
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
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #316590;
          color: #FFFFFF;
        }
        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a {
          background-color: #2E3537;
          color: #FFFFFF;
        }
        /* other links in the sidebarmenu when hovered */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #438BC5;
        }
        /* body */
        .content-wrapper, .right-side {
          background-color: #ffffff; 
        }
        .row {
          vertical-align: middle;
        }
      '))),
      tabItems(
        Policies,
        Dashboard
      )
    )
  )
)
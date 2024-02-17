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
                fileInput("upload", label = NULL, accept = c(".csv")),
                hr(),
                div(class = "sidebar-text",
                    style = "margin-left: 20px;",
                    "Choose Policy File:"
                ),
                div(style = "margin-top: -20px;",
                    selectizeInput("pick_policy", "",
                                   choices = '',
                                   multiple = FALSE,
                                   width = "100%"),
                ),
                div(style = "display: flex; margin-left:17px;",
                    actionButton("upload_json", "Upload Policy File"),
                    actionButton("delete_json", "", icon = icon("trash-can"))
                ),
                br(),
                hr(),
                div(style = "display: flex; align-items: center; margin-left: 15px;",
                    h5("Download Your Yaml Policy File:")
                ),
                downloadButton("download_policy_file","Download Policy File")
                # div(style = "display: flex; align-items: center; margin-left: 15px;",
                #     downloadButton("download_policy_file","Download Policy File")
                # )
            )
        ),
        dashboardBody(
            tabItems(
                Files,
                Policies
                )
        )
    )
)
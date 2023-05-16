
library(shinydashboard)
#Create UI Comps
UICompDirectory <- "UIComponents/"

source(paste0(UICompDirectory, "Policies.R"), local = TRUE)
source(paste0(UICompDirectory, "Dashboard.R"), local = TRUE)

shinyUI(
    dashboardPage(
        dashboardHeader(title = "Gradebook"),
        
       
      
        dashboardSidebar(
            ## Sidebar content
            sidebarMenu(
                menuItem("Policies", tabName = "policies", icon = icon("th")),
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
               
            )
            
        ),
        ## Body content
        dashboardBody(
            tabItems(
                # First tab content
                Dashboard,
                # Second tab content
                Policies
  
            )
        ))
  
)
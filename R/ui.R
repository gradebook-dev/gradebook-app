

#Create UI Comps
UICompDirectory <- "UIComponents/"

source(paste0(UICompDirectory, "Policies.R"), local = TRUE)
source(paste0(UICompDirectory, "Dashboard.R"), local = TRUE)

shinyUI(
  fluidPage(
            tags$style(".row{height: 100vh;} .row div:nth-child(1){height: 100%;}"),
            navlistPanel(widths = c(2,10),
                "Gradebook",
                Policies,
                Dashboard
                
            )
  
  
  )
)


#Create UI Comps
UICompDirectory <- "UIComponents/"

source(paste0(UICompDirectory, "Policies.R"), local = TRUE)
source(paste0(UICompDirectory, "Dashboard.R"), local = TRUE)

shinyUI(
  fluidPage(
            
            navlistPanel(widths = c(2,10),
                "Gradebook",
                Policies,
                Dashboard
                
            )
  
  
  )
)
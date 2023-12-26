exceptions_modal <- modalDialog(
    
    h4("Pick a Category:"),
    fluidRow(column(6,selectInput("cat_e", "Category Name", selected = "", 
                                  choices = "", width = "100%")
    )),
    h6("Insert More Criteria Here Later..."),
    fluidRow(
        column(6,
               selectInput("aggregation", "Aggregation:", selected = "equally_weighted",
                           choices = c("equally_weighted", "weighted_by_points", 
                                       "max_score", "min_score", "none"))),
        column(3,
               shinyWidgets::autonumericInput("weight", "", value = 0, currencySymbol = "%",
                                              currencySymbolPlacement = "s", width = "100px"),
               numericInput("num_lateness", label = "Number of Lateness Intervals:", value = 0)),
        column(3,
               numericInput("n_drops", label = "Number of Drops:", value = 0, min = 0),
               selectInput("clobber", "Clobber with:", selected = "None", choices = c("None"))
               )
    ),
    selectizeInput("assignments", "Select Assignments:",
                   choices = "", multiple = TRUE, width = "100%",
                   options = list(create = TRUE)),
    fluidRow(
        column(4,
               selectInput("pick_id", "Pick ID Column", choices = "", selected = "")
               ),
        column(8, 
               selectizeInput("students", "Select Studentd:",
                              choices = "", multiple = TRUE)
               )
    ),
    
    footer = tagList(
        actionButton("cancel_e", "Cancel"),
        actionButton("save_e", "Save"))
)


get_cat_names <- function(flat_policy){
    purrr::map(flat_policy$categories, "category") |> unlist() |> sort()
}

get_index_from_nr <- function (flat_policy, name){
    names <- purrr::map(flat_policy$categories, "category") |> unlist()
    which(names == name)
}
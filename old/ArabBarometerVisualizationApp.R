#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(survey)
#source("C:/Users/Patrick Harned/Documents/Documents/Arab-Barometer-Team/Wraps/Functions.R")
#dir =paste(paste("/", paste(strsplit(getwd(), "/")[[1]][2:6], collapse = "/"), sep=""), "/Wraps/Functions.R", sep = "")
#source(dir)
abv=abv_en
mylist =  names(abv)[  startsWith(names(abv_en), "Q")& names(abv_en)%nin%c("Q1001")  ]

# Name it
names(mylist) <-names(abv)[  startsWith(names(abv_en), "Q") & names(abv_en)%nin%c("Q1001") ]

library(shiny)
design = svydesign(ids = ~id, weights = ~wt, strata = ~stratum, data = filter(abv,!is.na(wt)), nest = TRUE)
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Arab Barometer Visualization Tool"),
    
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "variable",
                        "Choose a Variable",
                        choices = mylist),
            actionButton(inputId = "save", label = "Save Plot"),
            actionButton(inputId = "render", label = "Generate Plot"),
            actionButton(inputId = "country", label = "By Country")
        ),
        
        plotOutput("plot")
    )
)

server = function(input, output) {
  
  
  rv = reactiveValues(
    data =  svytable(reformulate(input$variable), design = design))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
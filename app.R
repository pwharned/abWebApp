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
library(haven)
library(readxl)
library(ggplot2)
library(expss)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
source("Functions.R")
#dir =paste(paste("/", paste(strsplit(getwd(), "/")[[1]][2:6], collapse = "/"), sep=""), "/Wraps/Functions.R", sep = "")
#source(dir)
abv=abv_en
mylist =  names(abv)[  startsWith(names(abv_en), "Q")& names(abv_en)%nin%c("Q1001","Q1001GCC", "Q1002")  ]

# Name it
names(mylist) <-names(abv)[  startsWith(names(abv_en), "Q") & names(abv_en)%nin%c("Q1001", "Q1001GCC", "Q1002") ]

library(shiny)displ
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
            actionButton(inputId = "country", label = "By Country"),
    radioButtons(input = "filetype", label = "Select the file type", choices = list("png")), 
            downloadButton(outputId = "down", label = "Download the plot")), 
    mainPanel = mainPanel(plotOutput("plot"))),
    

    tableOutput("table"),
  
    
)

server = function(input, output) {
  
  abtable = reactive({
    
    svytable(reformulate(input$variable), design = design)%>%
      as_tibble()%>%
      mutate(Percent = n/sum(n))%>%
      mutate(Percent = round(Percent*100))%>%
      mutate(Labels = names(val_lab(abv[[input$variable]])  [val_lab(abv[[input$variable]])%in%unique(abv[[input$variable]])     ]    )           )%>%
      select(Labels, Percent )%>%
      `colnames<-`(c(input$variable, "Percent"))
  })
  

abplot = reactive({
  
  var = sym(input$variable)
  var = enquo(var)
  
  abtable()%>%ggplot(aes(reorder(!!var, Percent), Percent))+geom_col( fill = '#7CBBC7')+scale_y_continuous(labels = c("","25","50","75","100"), limits = c(0,100))+
    coord_flip()+geom_text(aes(label = Percent),size=7, hjust=-1)+
    theme_bw()+
    theme(legend.position = "none", 
          axis.title = element_blank(),
          axis.text.x = element_text(size = 20),
          plot.caption = element_text(size = 25, hjust = 1, vjust=1),
          legend.title = element_blank(),
          plot.title = element_text(size=30, hjust=0.5, face = 'bold', color = '#545454', vjust = 2),
          plot.subtitle = element_blank(),
          axis.text.y = element_text(size = 25, angle = 45),
          text = element_text(size = 25),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          plot.margin=unit(c(1,1,1,1),"cm"),
          panel.grid.minor = element_blank())+ggtitle(title_function(input$variable))
  
  
  
})
  
  output$table = renderTable({
    
    svytable(reformulate(input$variable), design = design)%>%
      as_tibble()%>%
      mutate(Percent = n/sum(n))%>%
      mutate(Percent = round(Percent*100))%>%
      mutate(Labels = names(val_lab(abv[[input$variable]])  [val_lab(abv[[input$variable]])%in%unique(abv[[input$variable]])     ]    )           )%>%
      select(Labels, Percent )%>%
      `colnames<-`(c(input$variable, "Percent"))
    
  })
  
  
  
  
  output$plot <- renderPlot({
    

    abplot()
    
  })
  
  
  
  
  output$down =  downloadHandler(
    filename = function(){
      paste(input$variable, input$filetype, sep = ".")
    },
    content = function(file){
      
      
      ##open the device
      ##create the plot
      ## close the device
      ggsave(file, abplot(), device = "png", width = 12, height = 12)
      
      
      
      
      
    }
  )


  
}

# Run the application 
shinyApp(ui = ui, server = server)
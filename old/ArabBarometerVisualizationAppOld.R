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
mylist =  names(abv)[  startsWith(names(abv_en), "Q")  ]

# Name it
names(mylist) <-names(abv)[  startsWith(names(abv_en), "Q")  ]

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
    data =  svytable(reformulate(input$variable), design = design)%>%
      as_tibble()%>%
      mutate(Percent = n/sum(n))%>%
      mutate(Percent = round(Percent*100))%>%
      mutate(Labels = names(val_lab(abv[[input$variable]])  [val_lab(abv[[input$variable]])%in%unique(abv[[input$variable]])     ]    )           )%>%
      select(Labels, Percent )%>%
      `colnames<-`(c(input$variable, "Percent"))%>%ggplot(aes(!!data(), Percent))+geom_col( fill = '#7CBBC7')
  ) 
  
  observeEvent(input$country, {rv$data  = svytable(reformulate(c(input$variable,"Country")), design = design)%>%
    as_tibble()%>%
    mutate(Percent = n/sum(n))%>%
    mutate(Percent = round(Percent*100))%>%
    group_by(Country)
    mutate(Labels = names(val_lab(abv[[input$variable]])  [val_lab(abv[[input$variable]])%in%unique(abv[[input$variable]])     ]    )           )%>%
    select(Country,Labels, Percent )%>%
    `colnames<-`(c("Country", input$variable, "Percent")) })%>% ggplot(aes(!!data(), Percent))+geom_col( fill = Country)
    
  
   data = eventReactive(input$render,{
        quote = sym(input$variable)
        quote = enquo(quote)
    })
    
    
    output$plot <- renderPlot({
    
          
            rv$data+scale_y_continuous(labels = c("","25","50","75","100"), limits = c(0,100))+
            coord_flip()+geom_text(aes(label = Percent),size=7, hjust=-1)+
            theme_bw()+
            theme(legend.position = "none", 
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
            mutate(Percent = round(Percent*100))
    })
    
    observeEvent(input$save, { print("Plot Saved!")})
}

# Run the application 
shinyApp(ui = ui, server = server)
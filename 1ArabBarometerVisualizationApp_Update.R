library(shiny)
library(survey)
#source('/Users/pharned/Documents/Arab_Barometer_Team/Wraps/Functions.R')
abv=recode_country(abv_en)
mylist =  names(abv)[  startsWith(names(abv_en), "Q")  ]

# Name it
names(mylist) <-names(abv)[  startsWith(names(abv_en), "Q")  ]

design = svydesign(ids = ~id, weights = ~wt, strata = ~stratum, data = filter(abv,!is.na(wt)), nest = TRUE)



ui = fluidPage(
    
    fluidRow(
        tags$h1("Arab Barometer Visualization Tool"),
        column(2,selectInput(inputId = "variable",
                    "Choose a Variable",
                    choices = mylist)),
        column(3,actionButton(inputId = "overall", label = "Overall")),
        column(4,actionButton(inputId = "country", label = "Country")),
        column(5,actionButton(inputId = "pie", label = "Pie")),
        column(5, textInput('filename', "Filename")),
        column(6, downloadButton('savePlot', "Check to save")), 
        column(7, selectInput(inputId = "plotType", "Choose a Plot Type", choices = c("country", "overall", "graph")))
    ),
   
    fluidRow(  column(1,plotOutput("hist", height = 1000, width = 1000)),
               column(2,offset = 8, tableOutput("table")    ))
    
)


server = function(input, output){
    data = reactive({
        svytable(reformulate(c(input$variable)), design= design)%>%
            as_tibble()%>%
            mutate(Percent = n/sum(n))%>%
            mutate(Percent = round(Percent*100))%>%
            mutate(Labels = names(val_lab(abv[[input$variable]])  [val_lab(abv[[input$variable]])%in%unique(abv[[input$variable]])     ]    )           )%>%
            select(Labels, Percent )%>%
            `colnames<-`(c( input$variable, "Percent"))
    })
    
    rv = reactiveValues(
        data  = rnorm(100)
                        )
    
    observeEvent(input$overall, {
        
        quote = sym(input$variable)
        quote = enquo(quote)
        
        rv$data = svytable(reformulate(c(input$variable)), design = design)%>%
            as_tibble()%>%
            mutate(Percent = n/sum(n))%>%
            mutate(Percent = round(Percent*100))%>%
            mutate(Labels = names(val_lab(abv[[input$variable]])  [val_lab(abv[[input$variable]])%in%unique(abv[[input$variable]])     ]    )           )%>%
            select(Labels, Percent )%>%
            `colnames<-`(c( input$variable, "Percent"))%>%
            ggplot(aes(reorder(!!quote, Percent), Percent))+geom_col( fill = '#7CBBC7', position = position_dodge())+theme(legend.position = "none")+geom_text(aes(label=Percent),size=5, position = position_dodge(width = 1), hjust = -.5)+coord_flip()+
          theme_bw()+
          theme( 
            axis.text.x = element_text(size = 20),
            plot.caption = element_text(size = 25, hjust = 1, vjust=1),
            legend.title = element_blank(),
            plot.title = element_text(size=30, hjust=0.5, face = 'bold', color = '#545454', vjust = 2),
            plot.subtitle = element_blank(),
            axis.text.y = element_text(size = 25, angle = 45),
            text = element_text(size = 25, color = "#545454", family = "Montserrat"),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin=unit(c(3,3,3,3),"cm"),
            panel.grid.minor = element_blank())+ggtitle(title_function(input$variable))+xlab("")+ylab("")+scale_y_continuous(limits = c(0,100), breaks = c(25,50,75))
        
    })
    
    observeEvent(input$country,{
        
        quote = sym(input$variable)
        quote = enquo(quote)
        
        rv$data = svytable(reformulate(c(input$variable,"Country")), design = design)%>%
            as_tibble()%>%
            group_by(Country)%>%
            mutate(Percent = n/sum(n))%>%
            mutate(Percent = round(Percent*100))%>%
            group_by(Country)%>%
            mutate(Labels = names(val_lab(abv[[input$variable]])  [val_lab(abv[[input$variable]])%in%unique(abv[[input$variable]])     ]    )           )%>%
            select(Country, Labels, Percent )%>%
            filter(Percent!=0)%>%
            `colnames<-`(c("Country", input$variable, "Percent"))%>%
            ggplot(aes(reorder(Country,-Percent), Percent, group=reorder(!!quote, Percent)))+geom_col( aes(fill = !!quote), position = position_dodge())+scale_fill_ab("test1", reverse = TRUE)+geom_text(aes(label=Percent),size=5, position = position_dodge(width = 1), hjust = -.5)+
          coord_flip()+
          theme_bw()+
          theme( 
            axis.text.x = element_text(size = 20),
            plot.caption = element_text(size = 25, hjust = 1, vjust=1),
            legend.title = element_blank(),
            plot.title = element_text(size=30, hjust=0.5, face = 'bold', color = '#545454', vjust = 2),
            plot.subtitle = element_blank(),
            axis.text.y = element_text(size = 25, angle = 45),
            text = element_text(size = 25, color = "#545454", family = "Montserrat"),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin=unit(c(3,3,3,3),"cm"),
            panel.grid.minor = element_blank())+ggtitle(title_function(input$variable))+xlab("")+ylab("")+scale_y_continuous(limits = c(0,100), breaks = c(25,50,75))
        
        
 
        
    })
    
  
    
output$savePlot = downloadHandler(
  filename = function(){
    paste(input$variable, 'png', sep = ".")
  },
  content = function(file){
    ggsave(plot = rv$data, filename = paste(input$variable, 'png', sep = ".")
           )
    
  }
)
    
    
    output$hist = renderPlot({
        
        rv$data
       
        
        
    })
    
    
 
    
    output$table = renderTable({
        data()
    })
}




# Run the application 
shinyApp(ui = ui, server = server)



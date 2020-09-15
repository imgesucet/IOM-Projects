# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

#data 

asylum<-read.csv('asylumdata_new.csv')
data("countries")
migration<-read.csv('migrationdata_new.csv')


ui<-fluidPage(
    titlePanel("Google Trends Index"),
    sidebarLayout(sidebarPanel(
        #select country
        selectInput("geo", label = h5("Select Country"),
                    choices = list("US","SY"), selected = "SY"),
        selectInput("time", label=h5("Date"),
                    choices = list("all",'today+5-y'), selected = "all"),
        selectInput("lang", label=h5("language"),choices= list('ab','en','fr'),selected = "en"),
        
        selectInput("queries", label = h5("Destination"),
                    choices=list("Syria","Germany"),selected = "Germany" ),
        selectizeInput("Migration.origin", label = h5("Migration Data origin"),
                       choices=unique(migration$Country.of.birth.nationality.stand),selected = "Germany" ),
        selectizeInput("Migration.destination", label = h5("Migration Destination"),
                       choices=unique(migration$Country.stand),selected = "Germany"),
        selectizeInput("Asylum.destination", label = h5("Asylum Destination"),
                       choices=unique(asylum$Country.of.asylum.stand),selected = "Germany"),
        selectizeInput("Asylum.origin", label = h5("Asylum Origin"),
                       choices=unique(asylum$Country.of.origin.stand),selected = "Germany")),
        
        #puts all plots on different tabs. Maybe we can add graphs here
        mainPanel(      tabsetPanel(
            tabPanel("Plot1",plotOutput(outputId = "plot1",height=500)),
            tabPanel("Plot2", plotOutput(outputId = "plot2",height=500)),
            tabPanel("Plot3",plotOutput(outputId = "plot3",height=500))
        )
        )
        
    )
)




## server.R ##
library(shiny)
library(shinydashboard)
library(gtrendsR)


server<-function(input, output) {
    
    
    data("countries")
    
    dataInput <- reactive({
        dashboard(keyword = input$queries,
                  geo = input$geo,
                  time = input$time,
                  hl= input$lang)
    })
    
    output$plot1 <- renderPlot({
        #plot for all
        res <- dataInput()
        plot(res)
    })
    dataInput1<- reactive({
        a<- migration %>%
            filter(Country.of.birth.nationality.stand == input$Migration.destination)
        
    })
    ouput$plot2<- renderPlot({
        ggplot(dataInput1(),aes(x= Year,Value))+
            geom_line()+
            theme_bw()+
            xlab("Year")+
            ylab("Number of Migration")+
            ggtitle("Migration flows over time")
    })
    dataInput2<- reactive({
        a<- asylum %>%
            filter(Country.of.asylum.stand == input$Asylum.destination)
    })  
    ouput$plot3<- renderPlot({
        ggplot(dataInput2(),aes(x= Year,applied))+
            geom_line()+
            theme_bw()+
            xlab("Year")+
            ylab("Number of Migration")+
            ggtitle("Migration flows over time")
    })
    
}

shinyApp(ui=ui,server=server)


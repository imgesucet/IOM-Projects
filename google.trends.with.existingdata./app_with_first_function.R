library(shiny)
library(plotly)
library(gtrendsR)
library(readr)
library(ggplot2)
library(dplyr)

asylum<-read.csv('asylumdata_new.csv')
source("Function_Rshiny.R")
data("countries")
migration<-read.csv('migrationdata_new.csv')

ui<-fluidPage(
    titlePanel("Google Trends Index"),
    sidebarLayout(
        sidebarPanel(
            #select country
            selectInput("geo", label = h5("Select Country"),
                        choices = list("US","SY"), selected = "SY"),
            sliderInput("Year", label=h5("Year for data"),
                        min = min(migration$Year),max = max(migration$Year), value = c(min(migration$Year),max(migration$Year)),ticks = T,round = T,step = 1),
            selectInput("lang", label=h5("language"),choices= list('ab','en','fr'),selected = "en"),
            selectInput("time", label=h5("time"),choices= list('all'),selected = "all"),
            
            selectInput("queries", label = h5("Destination"),
                        choices=list("Syria","Germany"),selected = "Germany" ),
            selectInput("Migration.origin", label = h5("Migration Data origin"),
                        choices=unique(migration$Country.of.birth.nationality.stand),selected = "Germany" ),
            selectInput("Migration.destination", label = h5("Migration Destination"),
                        choices=unique(migration$Country),selected = "Germany"),
            selectInput("Asylum.destination", label = h5("Asylum Destination"),
                        choices=unique(asylum$Country.of.asylum),selected = "Germany"),
            selectInput("Asylum.origin", label = h5("Asylum Origin"),
                        choices=unique(asylum$Country.of.origin),selected = "Germany")),
        
        #puts all plots on different tabs. Maybe we can add graphs here
        mainPanel( plotOutput("plot1",height=500)
                   
                   
                   
        )
        
    )
)




## server.R ##
library(shiny)
library(shinydashboard)
library(gtrendsR)
library(ggplot2)
library(stringr)
library(ggpubr)
library("dplyr")
library("stringr")
library("tidyverse")
library(ggpubr)


server<-function(input, output) {
    
    
    data("countries")

    dataInput <- reactive({
        Generate_comparison(serach_terms = input$queries,
                            origin_code = input$geo,
                            language = input$lang,
                            origin = input$Migration.origin,
                            destination = input$Migration.destination,
                            origin_asylum = input$Asylum.origin,
                            destination_asylum = input$Asylum.destination)
    })
    output$plot1<- renderPlot({
        dataInput()
        
        
        
    })
    
}

shinyApp(ui=ui,server = server)


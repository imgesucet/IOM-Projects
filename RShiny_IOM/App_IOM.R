

# Source ------------------------------------------------------------------
source("Functions_RShiny.R")


# User Interface ----------------------------------------------------------

ui<-fluidPage(
  
  titlePanel("Google Trends Index"),
  sidebarLayout(
    sidebarPanel(width = 3,
      
      selectInput("origin_code", label = h5("Origin Country"),
                  choices = list("SY", "VE"), selected = "SY"),
      
      selectInput("search_terms", label = h5("Serach key word"),
                  choices=unique(Migrationdata$Country),selected = "Germany"),
      
      selectInput("lang", label=h5("language"),choices= list('ab','en','fr','es'),
                  selected = "en"),
      
      selectInput("origin", label = h5("Origin of Migrants"),
                  choices=unique(Migrationdata$Country.of.birth.nationality),selected = "Syria" ),
      
      selectInput("destination", label = h5("Destination of Migrants"),
                  choices=unique(Migrationdata$Country),selected = "Germany" ),

      
      selectInput("origin_asy", label = h5("Asylum Origin"),
                  choices=list(unique(asylum_applications_UNHCR$`Country of origin`)),selected = "Syrian Arab Rep."),
      
      selectInput("destination_asy", label = h5("Asylum Destination"),
                  choices=list(unique(asylum_applications_UNHCR$`Country of asylum`)),selected = "Germany")),
      
    #puts all plots on different tabs. Maybe we can add graphs here
    mainPanel( plotOutput("mainplot",height=700))
    )
  )



# Server ------------------------------------------------------------------

server<-function(input, output) {
  # data("countries")

  dataInput <- reactive({
    Dashboard_DT(Migrationdata,
                 asylum_applications_UNHCR,
                 origin = input$origin,
                 destination = input$destination,
                 Year_from = 2005,
                 Year_till = 2015,
                 origin_asylum = input$origin_asy,
                 destination_asylum = input$destination_asy,
                 serach_terms = input$search_terms,
                 origin_code = input$origin_code,
                 language = input$lang,
                 category = 555)
  })
  output$mainplot<- renderPlot({
    
    Dashboard_plot(dataInput())



  })

}


# Run ---------------------------------------------------------------------
shinyApp(ui,server)


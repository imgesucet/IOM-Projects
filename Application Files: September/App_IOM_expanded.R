<<<<<<< HEAD


# Source ------------------------------------------------------------------
source("Functions_RShiny.R")
source("Functions_RShiny.R")


# User Interface ----------------------------------------------------------

ui<-fluidPage(
    
    titlePanel("Google Trends Index"),
    sidebarLayout(
        sidebarPanel(width = 3,
                     
                     # selectInput("origin_code", label = h5("Origin Country"),
                     #             choices = list("SY", "VE"), selected = "SY"),
                     selectInput("origin_code", label = h5("Origin Country"),
                                 choices = Countries_gtrends$country_name, selected = "SYRIA"),
                     
                     selectInput("search_terms", label = h5("Serach key word"),
                                 choices=unique(Migrationdata$Country),selected = "Germany"),
                     
                     selectInput("lang", label=h5("language"),
                                 choices= list('ab','en','fr','es'),selected = "en"),
                     
                     selectInput("origin", label = h5("Origin of Migrants"),
                                 choices=unique(Migrationdata$Country.of.birth.nationality),selected = "Syria" ),
                     
                     selectInput("destination", label = h5("Destination of Migrants"),
                                 choices=unique(Migrationdata$Country),selected = "Germany" ),
                     #______ Migration keywords for top searches -------#
                     
                     selectInput("migration.keyword", label = h5("Select Keyword for top hits globaly"),
                                 choices=c("migration","asylum"),selected = "migration"),
                     
                     #______ input to select for top hits around the world in regards to time-------#
=======
# Source -------------------------------------------------------------------
source("Functions_Rshiny_Imgesu.R")
source("Functions_RShiny_Murtiza.R")



# User Interface ----------------------------------------------------------
ui<-fluidPage(theme = shinytheme("cerulean"),
              titlePanel("Google Trends Index"),
              sidebarLayout(
                  sidebarPanel(
                      
                      #______ input to select 'origin' AKA geolocation of search-------#
                      
                      selectInput("geo", label = h5("Origin code"),
                                  choices = unique(Geo_vs_country_origin1$country), selected = "Syria"),
                      
                      #______ language option -------#
                      
                      # selectInput("lang", label=h5("Origin language"),
                      #             choices= unique(iso$hl),selected = "en"),
                      
                      #______ search term AKA all countries as destination-------#
                      
                      selectInput("destination", label = h5("Destination Country"),
                                  choices=unique(Migrationdata$Country), selected = "Germany" ),
                     
                       #______ input to select for top hits around the world-------#
                      
                      # selectInput("Migration.origin", label = h5("Origin Country"),
                      #             choices=unique(Migrationdata$Country.of.birth.nationality),selected = "Syria" ),
                      # 
                      #______ Migration keywords for top searches -------#
                      
                      selectInput("migration.keyword", label = h5("Select Keyword for top hits globaly"),
                                  choices=c("migration","asylum"),selected = "migration"),
                      
                      #______ input to select for top hits around the world in regards to time-------#
>>>>>>> 14cc3dc08b3bb1abb4a0d8ae6d04d91b00e3c980
                     
                     selectInput("time.map", label = h5("Select time for top hits globally "),
                                 choices = c("now 1-H",
                                             "now 4-H"),
                                 selected = "now 4-H")),
        
        #____________________________main panel output ____________________# 
        mainPanel( tabsetPanel(type = "tabs",
                               tabPanel("Plots", plotOutput("plot1",height=700)),
                               tabPanel("Top Hits Map",plotOutput("map")),
                               tabPanel("Top Hits table",dataTableOutput("mytable")))
                   
        ) #ends main panel  
    )#ends side bar part
    
)#ends the fluid page




# Server ------------------------------------------------------------------

server<-function(input, output) {
    # data("countries")
    
    dataInput <- reactive({
        Dashboard_DT(Migrationdata,
                     asylum_applications_UNHCR,
<<<<<<< HEAD
                     origin = input$origin,
                     destination = input$destination,
                     Year_from = 2005,
                     Year_till = 2018,
                     origin_asylum = input$origin,
                     destination_asylum = input$destination,
                     serach_terms = input$search_terms,
                     origin_code = input$origin_code,
                     language = input$lang,
                     category = input$category)
    })
    #plots output 1
=======
                     origin = input$geo,
                     destination = input$destination,
                     Year_from = 2005,
                     Year_till = 2018,
                     origin_asylum = input$geo,
                     destination_asylum = input$destination,
                     serach_terms = input$destination,
                     origin_code = input$geo,
                     language = "en",
                     category = "Immigration Policy & Border Issues")})
    
    #this uses the first original function
    # Generate_comparison(serach_terms = input$queries,
    #                      origin_code = input$geo,
    #                       language = input$lang,
    #                      origin = input$Migration.origin,
    #                      destination = input$Migration.destination,
    #                     origin_asylum = input$Asylum.origin,
    #                     destination_asylum = input$Asylum.destination)
    #plots our function of 12 plots
>>>>>>> 14cc3dc08b3bb1abb4a0d8ae6d04d91b00e3c980
    output$plot1<- renderPlot({
        
        Dashboard_plot(dataInput())
        
        
        
    })
    output$map <- renderPlot({
        a<-top.hits(world,
                    search_keyword = input$migration.keyword,
                    time = input$time.map)
        
        top.hits.map(a)
    })
    data<- reactive({
        gtrends(keyword = input$migration.keyword,
                time = input$time.map)$interest_by_country%>%
            filter(location %in% world$region, hits>0)%>%
            mutate(region = location, hits = as.numeric(hits))%>%
            select(region,hits)%>%
            ungroup()%>%as.data.frame()
    })
    output$mytable <-renderDataTable({
        data()
        
    })
    
<<<<<<< HEAD
}#end server
=======
}#ends server function
>>>>>>> 14cc3dc08b3bb1abb4a0d8ae6d04d91b00e3c980

# Run ---------------------------------------------------------------------
shinyApp(ui,server)

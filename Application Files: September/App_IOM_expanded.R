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



# Server Side ----------------------------------------------------------

server<-function(input, output) {
    
    
    data("countries")
    #output of data table
    
    
    #this uses the newest function
    dataInput <- reactive({
        Dashboard_DT(Migrationdata,
                     asylum_applications_UNHCR,
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
    output$plot1<- renderPlot({
        res<-dataInput()
        Dashboard_plot(res)
        
        
        
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
    
}#ends server function

shinyApp(ui=ui,server = server)

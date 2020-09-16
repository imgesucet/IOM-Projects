
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
                      helpText("Options needed for second and third tab"),
                      
                      
                      selectInput("migration.keyword", label = h5("Select Keyword for top hits globaly"),
                                  choices=c("migration","asylum"),selected = "migration"),
                      
                      #______ input to select for top hits around the world in regards to time-------#
                     
                     selectInput("time.map", label = h5("Select time for top hits globally "),
                                 choices = c("now 1-H",
                                             "now 4-H",
                                             "now 1-d",
                                             "now 7-d",
                                             "today 1-m",
                                             "today 3-m",),
                                 selected = "now 4-H")),
        
        #____________________________main panel output ____________________# 
        mainPanel( tabsetPanel(type = "tabs",
                               tabPanel("Specification Plots", plotOutput("plot1",height=700),dataTableOutput("similarity.table"),textOutput("selected_var")),
                               tabPanel("Prediction", plotOutput("predict",height = 700)),
                               tabPanel("Top Hits Map",plotOutput("map")),
                               tabPanel("Top Hits table",dataTableOutput("mytable")))
                   
        ) #ends main panel  
    )#ends side bar part
    
)#ends the fluid page




# Server ------------------------------------------------------------------

server<-function(input, output) {

    #____________________first tab for ploting all specifications_________
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
    

    #_____plots our function of 12 plots____

    output$plot1<- renderPlot({
        
        Dashboard_plot(dataInput())
 })
    output$selected_var <- renderText({ 
        paste("Approach 1: Destination as keywords, category as Visa and Immigration for origin", input$geo, "to", input$destination)
        paste("Approach 2: Destination as keywords, without category for origin", input$geo, "to", input$destination)
        paste("Aprroach 3: Category as 555 Without any keywords for origin", input$geo, "to", input$destination)
        
    })
    #_____similarity score at the bottom of first specicification tab______
    
    output$similarity.table<- renderDataTable({
        #insert table for similartiy score
    })
    
    #create reactive data for prediction
    predict.data <- reactive({
        Dashboard_DT_without_norm(Migrationdata,
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
                                  category = "Immigration Policy & Border Issues")
        
    })
    #______________second tab for prediction_____________________________
    output$predict <- renderPlot({
        #put function here to render plot
        Predicting_migration(predict.data)
    })
    #__________________third tab to render map of top hits____________________
    output$map <- renderPlot({
        a<-top.hits(world,
                    search_keyword = input$migration.keyword,
                    time = input$time.map)
        
        top.hits.map(a)
    })
    #_________________fourth tab to show data table of top hits___________________
    
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
    
    

}#end server




# Run ---------------------------------------------------------------------
shinyApp(ui,server)

# Source ------------------------------------------------------------------
source("Functions_Rshiny_Imgesu.R")



# User Interface ----------------------------------------------------------




server<-function(input, output) {
    
    
    data("countries")
    #output of data table
    
    
    #this uses the newest function
    dataInput <- reactive({
        Dashboard_DT(Migrationdata,
                     asylum,
                     origin = input$Migration.origin,
                     destination = input$queries,
                     Year_from = 2005,
                     Year_till = 2018,
                     origin_asylum = input$Migration.origin,
                     destination_asylum = input$queries,
                     serach_terms = input$queries,
                     origin_code = input$geo,
                     language = input$lang,
                     category = 555)})
    
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
    
}#ends server functioin

shinyApp(ui=ui,server = server)

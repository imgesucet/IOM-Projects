library(shinydashboard)

ui<-dashboardPage(
    dashboardHeader(title = "Google Trends"),
    dashboardSidebar(sidebarMenu(
        menuItem("Chart", tabName = "chart", icon = icon("bar-chart")),
        menuItem("Data", tabName = "data", icon = icon("table")),
        selectInput("country", label = h5("Select Country"),
                    choices = list("Germany"="Germany","Syria"="Syria"), selected = "Syria"),
        selectInput("daterange", label=h5("Date"),
                    choices = list("all",'today+5-y'), selected = "all")
        ),
        selectInput("queries", label = h5("Search Queries"),
                   choices=list("Syria","Germnay"),selected = "Germany"
        ),
    
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "chart",
                    plotOutput("plot1", height = 500)
            ),
            
        )
    )
)




## server.R ##
library(shiny)
library(shinydashboard)
library(gtrendsR)


server<-function(input, output) {
    yesterday <- Sys.Date()-1
    
    
    dataInput <- reactive({
        gtrends(keyword = input$queries,
                geo = input$country,
                time = input$daterange)
    })
    
    output$plot1 <- renderPlot({
        res <- dataInput()
        plot(res)
    })
    

    
}

shinyApp(ui,server)


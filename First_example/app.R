library(shiny)
library(shinymaterial)
library(plotly)
library(gtrendsR)

ui <- material_page(title = "Google Trends using gtrends",
                    tags$br(),
                    material_row(
                        material_column(
                            width = 2,
                            material_card(
                                title = "",
                                depth = 4,
                                
                                material_text_box(
                                    input_id = "vec1",
                                    label = "Enter Trends",
                                    color = "#ef5350"
                                ),
                                material_dropdown(
                                    input_id = "geography",
                                    label = "Country",
                                    choices = list(
                                        "Worldwide",
                                        "Afghanistan",
                                        "Albania",
                                        "Algeria",
                                        "Angola",
                                        "Argentina",
                                        "Armenia",
                                        "Australia",
                                        "Austria",
                                        "Azerbaijan",
                                        "Bahamas",
                                        "Bahrain",
                                        "Bangladesh",
                                        "Belarus",
                                        "Belgium",
                                        "Botswana",
                                        "Brazil",
                                        "Bulgaria",
                                        "Burkina Faso",
                                        "Burundi",
                                        "Cambodia",
                                        "Cameroon",
                                        "Canada",
                                        "Chad",
                                        "Chile",
                                        "China",
                                        "Colombia",
                                        "Cuba",
                                        "Cyprus",
                                        "Czech Republic",
                                        "Denmark",
                                        "Djibouti",
                                        "Ecuador",
                                        "Egypt",
                                        "Equatorial Guinea",
                                        "Eritrea",
                                        "Estonia",
                                        "Ethiopia",
                                        "Finland",
                                        "France",
                                        "Gabon",
                                        "Gambia",
                                        "Georgia",
                                        "Germany",
                                        "Ghana",
                                        "Greece",
                                        "Hong Kong",
                                        "Hungary",
                                        "Iceland",
                                        "India",
                                        "Indonesia",
                                        "Iran",
                                        "Iraq",
                                        "Ireland",
                                        "Israel",
                                        "Italy",
                                        "Jamaica",
                                        "Japan",
                                        "Jordan",
                                        "Kazakhstan",
                                        "Kenya",
                                        "Kiribati",
                                        "Korea (North)",
                                        "Korea (South)",
                                        "Kuwait",
                                        "Kyrgyzstan",
                                        "Lebanon",
                                        "Liberia",
                                        "Libya",
                                        "Macedonia",
                                        "Madagascar",
                                        "Malawi",
                                        "Malaysia",
                                        "Mali",
                                        "Malta",
                                        "Mexico",
                                        "Morocco",
                                        "Mozambique",
                                        "Namibia",
                                        "Nepal",
                                        "Netherlands",
                                        "New Zealand",
                                        "Niger",
                                        "Nigeria",
                                        "Norway",
                                        "Oman",
                                        "Pakistan",
                                        "Paraguay",
                                        "Peru",
                                        "Philippines",
                                        "Poland",
                                        "Portugal",
                                        "Qatar",
                                        "Romania",
                                        "Russian Federation",
                                        "Rwanda",
                                        "Saudi Arabia",
                                        "Senegal",
                                        "Serbia",
                                        "Sierra Leone",
                                        "Singapore",
                                        "Somalia",
                                        "South Africa",
                                        "Spain",
                                        "Sudan",
                                        "Swaziland",
                                        "Sweden",
                                        "Switzerland",
                                        "Syria",
                                        "Taiwan",
                                        "Tajikistan",
                                        "Tanzania",
                                        "Thailand",
                                        "Togo",
                                        "Tunisia",
                                        "Turkey",
                                        "Turkmenistan",
                                        "Uganda",
                                        "Ukraine",
                                        "United Arab Emirates",
                                        "United Kingdom",
                                        "United States",
                                        "Uzbekistan",
                                        "Venezuela",
                                        "Viet Nam",
                                        "Yemen",
                                        "Zaire",
                                        "Zambia",
                                        "Zimbabwe"
                                    ),
                                    selected = "United States"
                                ),
                                material_dropdown(
                                    input_id = "period",
                                    label = "Time Period",
                                    choices = c(
                                        "Last day",
                                        "Last seven days",
                                        "Past 30 days",
                                        "Past 90 days",
                                        "Past 12 months",
                                        "Last five years"
                                    ),
                                    selected = "Last five years"
                                ),
                                submitButton("Submit")
                                
                                #
                                #         material_button(
                                #           input_id = "Submit",
                                #           label = "Update",
                                #           color = "deep-orange"
                                #         )
                            )
                        ),
                        material_column(
                            width = 9,
                            material_card(
                                title = "Google Trends",
                                depth = 4,
                                plotlyOutput("gtrends_plot")
                            )
                        )
                    ))

#server aspect 
server <- function(input, output, session) {
    ### Get Country Code
    library(gtrendsR)
    library(shinymaterial)
    library(plotly)
    geo <- reactive({
        if (input$geography == "Worldwide") {
            ""
        }
        
        else{
            countrycode(input$geography, 'country.name', 'iso2c')
        }
        
    })
    
    ### Time
    start_date <- reactive({
        if (input$period == "Last five years") {
            "today+5-y"
        }
        else if (input$period == "Past 12 months") {
            "today 12-m"
        }
        else if (input$period == "Past 90 days") {
            "today 3-m"
        }
        else if (input$period == "Past 30 days") {
            "today 1-m"
        }
        else if (input$period == "Last seven days") {
            "now 7-d"
        }
        else if (input$period == "Last  day") {
            "now 1-d"
        }
    })
    
    
    out <- reactive({
        if (length(input$vec1) > 0) {
            unlist(strsplit(input$vec1, ","))
        }
    })
    
    #### Eg : gtrends(keyword = NA, geo = "", time = "today+5-y")
    mk <- reactive({
        if (length(input$vec1 != 0))
            req(input$vec1)
        {
            gtrends(keyword = out(),
                    time = start_date(),
                    geo = geo())
            }
    })
    
    #Plot the Trend
    output$gtrends_plot <- renderPlotly({
        plot(mk())
    })
    
}
shinyApp(ui,server)

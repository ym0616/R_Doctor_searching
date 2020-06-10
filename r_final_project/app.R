library(shiny)
library(plotly)
library(jsonlite)
library(stringr)
library(RCurl)
library(magrittr)
library(purrr)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(shiny)
library(lubridate)
library(httr)
library(shinydashboard)
library(owmr)
library(jsonlite)
library(DT)

# # Global 
owmr_settings('0a779014ed1acf477929cabe0bff89d5')
#open_key<-'0a779014ed1acf477929cabe0bff89d5'
OWM_API_KEY<-'0a779014ed1acf477929cabe0bff89d5'
Sys.setenv(OWM_API_KEY = '0a779014ed1acf477929cabe0bff89d5') # if not set globally
my_key <-'230ee74b03a3b70f29fe260f8fa409c5'

#api_keys = read_json("../api.json")
#OWM_API_KEY = api_keys$OWM_API_KEY
#my_key = api_keys$WTS_API_KEY

r <- GET(
    "http://api.weatherstack.com/current",
        query = list(
            access_key = my_key,
            query = "Davis"

    )
)

stop_for_status(r)
json <- content(r, as = "text", encoding = "UTF-8")
weather <- fromJSON(json, flatten = TRUE)
weather$current$temperature
weather

#-------------------------------------------------------------
ui <- fluidPage(
 
    titlePanel(div(img(height = 75, width = 500, 
                       src = "https://stableisotopefacility.ucdavis.edu/images/ucdavis_logo_gold.png"))),
    
    theme = shinytheme("cerulean"),
    #dashboardHeader(title = "Weather & AQ App"),
    # shinythemes::themeSelector(),
    navbarPage(
        "Weather Report",
        tabPanel(
            "Davis Local Weather", 
            htmlOutput("text1"),
            actionButton("go", label = "Show Forecast Weather"),
            plotlyOutput("davis_plot"),
            DTOutput("forcasttable")
        ),
        tabPanel(
            "Search by Location",
            textInput(inputId = "loc", label = "Input a location", value = "Davis"),
            p('(E.g. "New York", "Los Angeles", "Seattle")'), 
            hr(),
            htmlOutput('tbl'),
            htmlOutput("data1"),
            tableOutput("tbl_forecast")
        ),
        tabPanel( 
            "See Historic Data", 
            sidebarLayout(
                sidebarPanel(
                    textInput(
                        inputId = "cityy",
                        label = "Select City:",
                        value = "Davis"
                    ),
                    p('(E.g. "New York", "Los Angeles", "Seattle")'),
                    hr(),
                    textInput(
                        inputId = "datee",
                        label = "Select Historic Date:",
                        value = '2019-09-21'
                    ),
                    p('(E.g. "2019-03-21", "2020-01-21", "2019-12-23" 
                      \n WARNING: DATE MUST BE AFTER 2019)'),
                    uiOutput("datee-valid"),
                    hr()
                ),
             mainPanel(
                 DTOutput("hour_data")
                 )
            )
        )
    )
)


server <- function(input, output, session) {
    
    ##############################################################################
    davis_r <- GET(
        "http://api.weatherstack.com/current",
        query = list(
            access_key = my_key,
            query = "Davis"
            
        )
    )
    stop_for_status(r)
    json <- content(r, as = "text", encoding = "UTF-8")
    weather <- fromJSON(json, flatten = TRUE)
    davis_data = weather$current$temperature
    whether <- as.data.frame(davis_data)
    #pollution <- as.data.frame(davis_data$data$current$pollution)
    
    output$text1 <- renderText({
        paste0(
            "<div style='position:relative;'>",
            "<img style='height: 380px;' src='https://www.timeanddate.com/scripts/weather_og.php?h1=Weather&h2=Local%20Weather%20Around%20the%20World'/>",
            "<div style='position:absolute; z-index:2; left:500px; top:3px;color:black;font-size:30px'>","Temp: ",
            davis_data, "°</div>", "<div style='position:absolute; z-index:2; left:408px; top:157px;color:black;font-size:25px'>",
            weather$wind_speed, #"</div>", "</div>",
            weather$current$weather_descriptions, "</div>", "</div>"
        )
    })
    
    output$tbl <- renderText({
        
        ## to require that the user types something, use: `req(input$loc)`
        req(input$loc != "", cancelOutput = TRUE)

        r = GET(
            "http://api.weatherstack.com/current",
            query = list(
                access_key = my_key,
                query = input$loc
                
            )
        )
        
        stop_for_status(r)
        json <- content(r, as = "text", encoding = "UTF-8")
        weather <- fromJSON(json, flatten = TRUE)
        input_data = weather$current$temperature
        
        paste(
            "Current Weather: ",
            weather$current$temperature, 
            "<br>Weather Descriptions: ",
            weather$current$weather_descriptions,
            "<br>Wind Degree: ",
            weather$current$wind_degree,
            "<br>wind Speed: ",
            weather$current$wind_speed,
            "<br>Current Pressure: ",
            weather$current$pressure,
            "<br>Current Humidity: ",
            weather$current$humidity,
            "<br>Current Cloudcover: ",
            weather$current$cloudcover,
            "<br>Current Visibility: ",
            weather$current$visibility
        )
        
    })
    
    output$tbl_forecast <- renderTable({
        
        ## to require that the user types something, use: `req(input$loc)`
        req(input$loc != "", cancelOutput = TRUE)
        forecastloc <- as.data.frame(get_forecast(input$loc, units = "metric")$lis)
        show(forecastloc[1:10, 1:9])

        })
    
    
    forecastdavis <- reactiveVal(NULL)
    
    
    observeEvent(input$go, {
        dat <- as.data.frame(get_forecast("Davis", units = "metric")$lis)
        forecastdavis(dat)

    })
    
    output$forcasttable <- renderDT({
        req(!is.null(forecastdavis()))
        forecastdavis()[1:10, 3:15]%>% 
            rename(
                 Temperature =  main.temp,
                 Date = dt_txt,
                 Feels_Like =main.feels_like,
                 Min_Temperature = main.temp_min,
                 Max_Temperature = main.temp_max,
                 Pressures = main.pressure,
                 Sea_Level = main.sea_level,
                 Ground_Level = main.grnd_level,
                 Humidity = main.humidity,
                 Temperature_kf = main.temp_kf,
                 Clouds = clouds.all,
                 Wind_Speed = wind.speed,
                 Wind_Degree = wind.deg
                 ) 
    })

    output$davis_plot <- renderPlotly({
        req(!is.null(forecastdavis()))
        forecastdavis() %>%
            rename(
                Temperature =  main.temp,
                Datetime = dt_txt
            ) %>% 
            ggplot(aes(ymd_hms(Datetime), Temperature, group=1),color = sex)+    #plot the summarised data
            geom_point()+                      
            geom_line()+
            labs( x = "DateTime")+
            ggtitle("Weather Forecast for Davis") + 
            theme(plot.title = element_text(face ="bold",size=40)) +
            theme_bw()
        
    })
    
    output$`datee-valid` = renderUI({
        if(!grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", input$datee)){
            tags$p("Date invalid. Please put Valid Date", style = "color: red;")
        }
    })
  
    
    output$hour_data <- renderDT({
        
        req(input$cityy != "", cancelOutput = TRUE)
        req(input$datee != "", cancelOutput = TRUE)
        
        if(!grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", input$datee)){
            return()
        }
        
        r2 <- GET(
            "http://api.weatherstack.com/historical",
            query = list(
                access_key = my_key,
                query = input$cityy,
                historical_date = input$datee,
                hourly = 1
            )
        )
       
        stop_for_status(r2)
        json2 <- content(r2, as = "text", encoding = "UTF-8")
        weather2 <- fromJSON(json2, flatten = TRUE)

        rr = weather2$historical[[input$datee]]$hourly[, 1:8] 
        rr = rr%>%
            mutate(
                weather_icons = glue::glue("<img src='{weather_icons}'></img>")
            ) 
        rr
    }, escape =FALSE)

   
}


shinyApp(ui, server)

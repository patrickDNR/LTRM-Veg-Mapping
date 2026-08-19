#Make a plot 

library(leaflet)
library(tidyverse)
library(lubridate)
library(sf)
library(paletteer)
library(shiny)
library(bslib)
library(viridis)

#download data
data_url <- "https://raw.githubusercontent.com/patrickDNR/LTRM-Veg-Mapping/refs/heads/main/Data/Pool8_AqVeg_SRS.csv"
download.file(data_url, 'Pool8_AqVeg_SRS.csv')

veg <- read.csv('Pool8_AqVeg_SRS.csv') %>%
  filter(SPPCD != 'NORAKE')

#add average rake score for the siteID for each species
veg$rake.avg <- rowMeans(veg[,66:71])

#Make a list of species
spp.list <- unique(veg$Common.Name)

# Define UI for water quality map app ----
ui <- bslib::page_sidebar(
  
  id = 'Aquatic Vegetation Data',
  
  # App title ----
  title = "Aquatic Vegetation Mapping",
  
  #theme
  theme = bslib::bs_theme(version = 5),
  
  #background color
  fillable_mobile = TRUE,
  
  #window title
  window_title = 'Veg data',
  
  #background color 
  bg = 'darkgreen',
  
  
  # Sidebar panel for inputs ----
  sidebar = 
    
    sidebar(
      sidebarPanel(
        width = 12,
        
        # Input: Let's try to do date range
        # Input: Maybe a slider for time series?
        sliderInput(inputId = 'date_range', 
                    label = 'Select Date Range:', 
                    min = min(veg$Year), 
                    max = max(veg$Year),
                    step = 1,
                    value = c(min(veg$Year), max(veg$Year)), 
                    sep = '', 
                    ticks = F),
        
        #Select months of interest
        checkboxGroupInput(inputId = 'months', 
                           label = 'Select Sampling Months:', 
                           choices = c( 
                             'June' = 'Jun', 
                             'July' = 'Jul', 
                             'August' = 'Aug'), 
                           selected = c('Jun', 'Jul', 'Aug')),
        
        
        #Input: Select constituent
        selectInput(inputId = 'VegSpecies', 
                    label = 'Species:', 
                    choices = spp.list),
        
        
        downloadButton(outputId = 'downloadData', 
                       label = 'Download CSV')
      )
    )
  ,
  
  # Main panel for displaying outputs ----
  
  navset_card_underline(
    
    nav_panel('About', 
              tags$img(height = 100, width = 100,
                       src = 'https://umesc.usgs.gov/ltrmp/images/buttons/veg-hi.jpg'),
              tags$html(
                tags$head(
                  tags$title('UMRR Long Term Resource Monitoring - Vegetation component')
                ),
                tags$body(
                  'Data from this visualization comes from the Long Term Resource
                    Monitoring element of the Upper Mississippi River Restoration program. The data displayed
                    are specific for Pool 8 of the UMR, collected by the La Crosse Field Station and the 
                    Wisconsin Department of Natural Resources in collaboration with USGS and US Army Corps of 
                    Engineers. For more information on the water vegetation component, including more data visualization options
                  and sampking methodology, please visit: ', a(
                      "the LTRM website",
                      target = "_blank",
                      href = "https://umesc.usgs.gov/data_library/vegetation/vegetation_page.html"
                    ), 
                  
                  tags$p("For more information on the river and Wisconsin's work on the UMR, 
                         please visit:", a('UMR by the Wisconsin DNR', 
                                           target = "_blank", 
                                           href = "https://dnr.wisconsin.gov/topic/UMR/About.html"))
                ), 
                
              )),
    
    
    nav_panel('Rake Score Time Series', plotOutput('vegBoxes', height = 500, width = 900), 
              'Boxes showing median, 25%, and 75% quantiles and whiskers showing 1.95x
              interquartile range of mean rake score for given species at a sample site.')
    ,
    
    
    # Output: Map of Veg variable ----
    
    nav_panel('Aquatic Vegetation sample point map',leafletOutput("vegMap", height = 800))
  )
  
  
)


# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste(input$VegSpecies)
  })
  
  
  filtered_data <- reactive({
    
    veg %>%
      filter(Year >= input$date_range[1] & Year <= input$date_range[2]) %>%
      filter(!is.na(rake.avg)) %>%
      filter(Month %in% input$months) %>%
      filter(Common.Name == input$VegSpecies) %>%
      filter(rake.avg > 0)
  })
  
  colorpal <- reactive({
    df <- filtered_data()
    
    colorNumeric('RdYlBu', domain = as.numeric(df$rake.avg), reverse = T)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  
  #Generate a boxplot across years
  output$vegBoxes <- renderPlot({
    df <- filtered_data()
    
    validate(
      need(nrow(df) > 0, 'No data available to display. Please select different Species.')
    )
    
    boxplot(
      df$rake.avg ~ df$Year, 
      xlab = 'Year',
      ylab = input$VegSpecies
    )
  })
  
  # Generate a plot of the requested variable in a pool 8 map
  output$vegMap <- renderLeaflet({
    df <- filtered_data()
    
    pal <- colorpal()
    
    chart <- df %>%
      leaflet() %>%
      addTiles() %>%
      setView(lng = -91.2, lat = 43.6, zoom = 12) %>%
      addCircleMarkers(data = df,
                       color = ~pal(df$rake.avg), 
                       popup = paste(df$DATE, '\n',input$VegSpecies,' = ', as.character(df$rake.avg)),
                       fillOpacity = 0.8, 
                       lat = df$lat, 
                       lng = df$lng) %>%
      addLegend(
        position = 'bottomright', 
        pal = pal, 
        values = ~df$rake.avg, 
        title = input$VegSpecies, 
        opacity = 1
      )
    
    chart
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste('AqgVegdata-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
}


shinyApp(ui, server)
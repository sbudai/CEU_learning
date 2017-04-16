#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#


source('/path/to/folder/CEU-R-lab_home_assignment_B/run.R') ## it needs to reset after copying into a library

shinyUI(
  
  fluidPage(
  
    titlePanel(h1(strong('Explore nycflights13 data'))), ## title of app
  
    sidebarLayout(
      sidebarPanel(
        dateRangeInput('date', ## inputId
                       'Date range', ## label
                       start = min(fli$date),
                       end = min(fli$date) + 30,
                       min = min(fli$date),
                       max = max(fli$date),
                       format = 'yyyy-mm-dd',
                       startview = 'month',
                       weekstart = 1, 
                       language = 'en', 
                       separator = ' to ', 
                       width = '260px'), 
        sliderInput('distance', ## inputId
                    'Distance (miles)', ## label
                    min = min(fli$distance),
                    max = max(fli$distance),
                    value = median(fli$distance),
                    ticks = TRUE,
                    ## animate = animationOptions(interval = 30, 
                    ##                            loop = TRUE, 
                    ##                            playButton = NULL, 
                    ##                            pauseButton = NULL),
                    width = '260px',
                    sep = ' '
                    ), ## a slider for setting distance
        checkboxGroupInput('show_dep_aip', 
                           'New Yorker airports:', 
                           c('JFK', 'LGA', 'EWR'),
                           selected = c('JFK', 'LGA', 'EWR')),
        width = 3),
      
    # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(h5('Explore punctuality'), 
                   h5(strong('Distance and Airtime')), plotOutput('distance_airtime'),
                   h5(strong('Distance and Departure Delay')), plotOutput('distance_dep_delay'),
                   h5(strong('Distance and Arrival Delay')), plotOutput('distance_arr_delay'),
                   h5(strong('Month of Year and Departure Delay')), plotOutput('month_dep_delay'),
                   h5(strong('Month of Year and Arrival Delay')), plotOutput('month_arr_delay')),
          tabPanel(h5('Flights from NY'), 
                   h5(strong('Density of flown distances from New Yorker airport(s)')), plotOutput('distance_dep'),
                   h5(strong('Density of flown air times from New Yorker airport(s)')), plotOutput('air_time_dep'),
                   h5(strong('Density of departure delays from New Yorker airport(s)')), plotOutput('delay_dep'),
                   h5(strong('Density of arrival delays for flights started from New Yorker airport(s)')), plotOutput('delay_arr')),
          tabPanel(h5('Carriers'), 
                   dataTableOutput('carriers')), ## output variable to read the table from
          tabPanel(h5('Aircraft models'), 
                   dataTableOutput('models')), ## output variable to read the table from
          tabPanel(h5('Departures'),
                   dataTableOutput('departures')), ## output variable to read the table from
          tabPanel(h5('Destinations'),
                   dataTableOutput('destinations')), ## output variable to read the table from
          id = NULL),	## If provided, you can use input$id in your server logic to determine which of the current tabs is active. The value will correspond to the value argument that is passed to tabPanel.
        width = 9),
      fluid = TRUE)
                   )
          )

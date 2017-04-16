#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#

source('/path/to/folder/CEU-R-lab_home_assignment_B/run.R') ## it needs to reset after copying into a library

shinyServer(function(input, output){
  
  get <- reactive({fli[which((date >= input$date[1]) & (date <= input$date[2]) & (distance <= input$distance) & (origin %in% input$show_dep_aip))]})
  
  ## ggplot is beautiful but extremly slow, so I changed my mind and switched to lattice ##
  output$distance_airtime <-    renderPlot({
                                    ## ggplot(get(), aes(distance, air_time)) +
                                    ## geom_point(aes(col = name)) + 
                                    ## labs(col = 'Departure\n airport') +
                                    ## theme_bw()
                                    xyplot(air_time ~ distance,
                                           get(),
                                           group = name,
                                           ylab = 'flown air time',
                                           auto.key = TRUE)
                                            })
  
  output$distance_dep_delay <-  renderPlot({
                                    ## ggplot(get(), aes(distance, dep_delay)) + 
                                    ## geom_point(aes(col = name))  + 
                                    ## labs(col = 'Departure\n airport') + 
                                    ## theme_bw()
                                xyplot(dep_delay ~ distance,
                                       get(),
                                       group = name,
                                       ylab = 'delays at departure',
                                       auto.key = TRUE)
                                            })  
  
  output$distance_arr_delay <-  renderPlot({
                                    ggplot(get(), aes(distance, arr_delay)) + 
                                    geom_point(aes(col = name))  + 
                                    labs(col = 'Departure\n airport') + 
                                    theme_bw()
                                xyplot(arr_delay ~ distance,
                                       get(),
                                       group = name,
                                       ylab = 'delays at arrival',
                                       auto.key = TRUE)
                                            })
  
  output$month_dep_delay <-     renderPlot({
                                    ## ggplot(get(), aes(month, dep_delay)) + 
                                    ## geom_point(aes(col = name))  + 
                                    ## labs(col = 'Departure\n airport') + 
                                    ## theme_bw()
                                xyplot(dep_delay ~ month,
                                       get(),
                                       group = name,
                                       ylab = 'delays at departure',
                                       auto.key = TRUE)
                                            })
  
  output$month_arr_delay <-     renderPlot({
                                    ## ggplot(get(), aes(month, arr_delay)) + 
                                    ## geom_point(aes(col = name))  + 
                                    ## labs(col = 'Departure\n airport') + 
                                    ## theme_bw()
                                xyplot(arr_delay ~ month,
                                       get(),
                                       group = name,
                                       ylab = 'delays at arrival',
                                       auto.key = TRUE)
                                            })
  
  output$distance_dep <-  renderPlot({
                              ## ggplot(get(), aes(distance)) + 
                              ## geom_density(aes(fill = name, position = 'stack'))  +
                              ## scale_fill_discrete(name = 'Departure\n airport') +
                              ## theme_minimal() + 
                              ## theme(legend.position = 'top')
                              densityplot( ~ distance,
                                          data = get(),
                                          groups = name,
                                          xlab = 'distance in miles',
                                          plot.points = FALSE,
                                          auto.key = TRUE)
                                      })
  
  output$air_time_dep <-  renderPlot({
                              ## ggplot(get(), aes(air_time)) + 
                              ## geom_density(aes(fill = name, position = 'stack')) +
                              ## scale_fill_discrete(name = 'Departure\n airport') +
                              ## theme_minimal() + 
                              ## theme(legend.position = 'top')
                          densityplot( ~ air_time,
                                       data = get(),
                                       groups = name,
                                       xlab = 'air_time in minutes',
                                       plot.points = FALSE,
                                       auto.key = TRUE)
                                      })
  
  output$delay_dep <-     renderPlot({
                              ## ggplot(get(), aes(dep_delay)) + 
                              ## geom_density(aes(fill = name, position = 'stack')) +
                              ## scale_fill_discrete(name = 'Departure\n airport') +
                              ## theme_minimal() + 
                              ## theme(legend.position = 'top')
                          densityplot( ~ dep_delay,
                                       data = get(),
                                       groups = name,
                                       xlab = 'departure delays in minutes',
                                       plot.points = FALSE,
                                       auto.key = TRUE)
                                       })
  
  output$delay_arr <-     renderPlot({
                              ## ggplot(get(), aes(arr_delay)) + 
                              ## geom_density(aes(fill = name, position = 'stack')) +
                              ## ## scale_fill_discrete(name = 'Departure\n airport') +
                              ## theme_minimal() + 
                              ## theme(legend.position = 'top')
                          densityplot( ~ arr_delay,
                                       data = get(),
                                       groups = name,
                                       xlab = 'arrival delays in minutes',
                                       plot.points = FALSE,
                                       auto.key = TRUE)
                                        }) 
  
  output$carriers <- renderDataTable({
                      get()[, .(Count = .N, 'avg. distance' = mean(distance, na.rm = TRUE), 'avg. air time' = mean(air_time, na.rm = TRUE), 'avg. departure delay' = mean(dep_delay, na.rm = TRUE), 'avg. arrival delay' = mean(arr_delay, na.rm = TRUE)), by = carrier_name]
                                      }, options = list(lengthMenu = c(5, 15, 100), pageLength = 15))
  
  output$models <- renderDataTable({ 
                      get()[, .(Count = .N, 'avg. distance' = mean(distance, na.rm = TRUE), 'avg. air time' = mean(air_time, na.rm = TRUE), 'avg. departure delay' = mean(dep_delay, na.rm = TRUE), 'avg. arrival delay' = mean(arr_delay, na.rm = TRUE)), by = model]
                                      }, options = list(lengthMenu = c(5, 15, 100), pageLength = 15))
  
  output$departures <- renderDataTable({
                      get()[, .(Count = .N, 'avg. distance' = mean(distance, na.rm = TRUE), 'avg. air time' = mean(air_time, na.rm = TRUE), 'avg. departure delay' = mean(dep_delay, na.rm = TRUE), 'avg. arrival delay' = mean(arr_delay, na.rm = TRUE)), by = name]
                                        }, options = list(lengthMenu = c(5, 15, 100), pageLength = 15))
  
  output$destinations <- renderDataTable({
                      get()[, .(Count = .N, 'avg. distance' = mean(distance, na.rm = TRUE), 'avg. air time' = mean(air_time, na.rm = TRUE), 'avg. departure delay' = mean(dep_delay, na.rm = TRUE), 'avg. arrival delay' = mean(arr_delay, na.rm = TRUE)), by = dest_name]
                                        }, options = list(lengthMenu = c(5, 15, 100), pageLength = 15))
                                  })
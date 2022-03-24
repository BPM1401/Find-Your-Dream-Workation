
# Define server logic required to create density plots for Parameters
shinyServer(function(input, output) {
  
  
  citylbl <- reactiveValues(clickedMarker=NULL)
  output$Geo2 <- renderLeaflet({
    
    leaflet(city) %>% addTiles() %>% addCircleMarkers(data=city, layerId = city$City,
                                                      radius=3, 
                                                    color = "red", opacity = 0.8,
                                                    popup = city$City,
                                                    clusterOptions = markerClusterOptions())
    
  })
  
  observeEvent(input$Geo2_marker_click,{
    print("observed map_marker_click")
    citylbl$clickedMarker <- input$Geo2_marker_click
    print(citylbl$clickedMarker)
    output$MapTable <- renderTable({
      return(citycompgeo[, c("Parameter", citylbl$clickedMarker$id)]
      )
    })
  })
  

  
  
  output$Geo <- renderPlot({
    ggplot() +
      geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
      geom_point(data = city, aes(x = lng, y = lat), color = 'red') + 
      ggtitle("147 Cities") + 
      theme(plot.title = element_text(hjust=0.5, size=21, face='bold', color='red')) +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      theme(axis.title.x=element_blank()) +
      theme(axis.title.y = element_blank())
  }, height = 500, width = 750)
  
  output$Box_Para <- renderPlot({
    
    box_title = x_axis %>%
      select(.data[[input$GeoPara]]) %>%
      filter(row_number()==1)
    
    x_long = x_axis %>%
      select(.data[[input$GeoPara]]) %>%
      filter(row_number()==2)
    
    
    min_x = min(df[,input$GeoPara])
    min_city = df %>% filter(.data[[input$GeoPara]] == min(.data[[input$GeoPara]])) %>%
      select(City) %>% pull()
    
    max_x = max(df[,input$GeoPara])
    max_city = df %>% filter(.data[[input$GeoPara]] == max(.data[[input$GeoPara]])) %>%
      select(City) %>% pull()
    
    med_x = median(df[,input$GeoPara])
    med_city = df %>% filter(.data[[input$GeoPara]] == median(.data[[input$GeoPara]])) %>%
      select(City) %>% pull()
    
    
    ggplot(df, aes(x=df[,input$GeoPara])) + 
      geom_boxplot(color="blue", outlier.color = "blue", outlier.size = 2) +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      ggtitle(box_title) +
      theme(plot.title=element_text(hjust=0.5, size=20, color='blue')) +
      labs(x=x_long) + 
      theme(axis.title.x=element_text(size=20, color='blue')) +
      theme(axis.text.x=element_text(size=20, color='blue')) + 
      scale_x_continuous(expand = c(0.13, 0)) +
      geom_text(x=max_x, y=0.14, label = max_x, size=5) + 
      geom_text(x=max_x, y=0.18, label = max_city, size=5) +
      geom_text(x=max_x, y=0.10, label = "(max)", size=4) +
      geom_text(x=min_x, y=-0.18, label = "(min)", size=4) +
      geom_text(x=min_x, y=-0.14, label = min_x, size=5) +
      geom_text(x=min_x, y=-0.10, label = min_city[1], size=5) +
      geom_text(x=med_x*1.08, y=0.00, label=med_x, size=5) + 
      geom_text(x=med_x*1.08, y=0.04, label=med_city[1], size=5) +
      geom_text(x=med_x*1.08, y=-0.04, label="(median)", size=4)
    
  })
  
  output$outwifi <- renderTable({
    
    is_outlier = function(x) {
      return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
    }
    
    df %>% 
      mutate(Outlier = ifelse(is_outlier(df[,'Wifi']), df[,'City'], 'False')) %>%
      filter(Outlier!='False') %>%
      select(City,Wifi) %>%
      arrange(desc(Wifi))

  })
  
  output$outCo <- renderTable({
    
    is_outlier = function(x) {
      return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
    }
    
    df %>% 
      mutate(Outlier = ifelse(is_outlier(df[,'Co_Work']), df[,'City'], 'False')) %>%
      filter(Outlier!='False') %>%
      select(City,Co_Work) %>%
      arrange(desc(Co_Work))
    
  })
  
  output$outCoffee <- renderTable({
    is_outlier = function(x) {
      return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
    }
    
    df %>% 
      mutate(Outlier = ifelse(is_outlier(df[,'Coffee']), df[,'City'], 'False')) %>%
      filter(Outlier!='False') %>%
      select(City,Coffee) %>%
      arrange(desc(Coffee))
  })
  
  output$outTaxi <- renderTable({
    is_outlier = function(x) {
      return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
    }
    
    df %>% 
      mutate(Outlier = ifelse(is_outlier(df[,'Taxi']), df[,'City'], 'False')) %>%
      filter(Outlier!='False') %>%
      select(City,Taxi) %>%
      arrange(desc(Taxi))
  })
  
  output$outBeer <- renderTable({
    is_outlier = function(x) {
      return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
    }
    
    df %>% 
      mutate(Outlier = ifelse(is_outlier(df[,'Beer']), df[,'City'], 'False')) %>%
      filter(Outlier!='False') %>%
      select(City,Beer) %>%
      arrange(desc(Beer))
  })
  
  output$outRent <- renderTable({
    is_outlier = function(x) {
      return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
    }
    
    df %>% 
      mutate(Outlier = ifelse(is_outlier(df[,'Rent']), df[,'City'], 'False')) %>%
      filter(Outlier!='False') %>%
      select(City,Rent) %>%
      arrange(desc(Rent))
  })
  
  output$outMeal <- renderTable({
    is_outlier = function(x) {
      return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
    }
    
    df %>% 
      mutate(Outlier = ifelse(is_outlier(df[,'Meal']), df[,'City'], 'False')) %>%
      filter(Outlier!='False') %>%
      select(City,Meal) %>%
      arrange(desc(Meal))
  })
  
  output$outSun <- renderTable({
    is_outlier = function(x) {
      return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
    }
    
    df %>% 
      mutate(Outlier = ifelse(is_outlier(df[,'Sun']), df[,'City'], 'False')) %>%
      filter(Outlier!='False') %>%
      select(City,Sun) %>%
      arrange(desc(Sun))
  })
  
  output$outFun <- renderTable({
    is_outlier = function(x) {
      return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
    }
    
    df %>% 
      mutate(Outlier = ifelse(is_outlier(df[,'Fun']), df[,'City'], 'False')) %>%
      filter(Outlier!='False') %>%
      select(City,Fun) %>%
      arrange(desc(Fun))
  })
  
  output$outInsta <- renderTable({
    is_outlier = function(x) {
      return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
    }
    
    df %>% 
      mutate(Outlier = ifelse(is_outlier(df[,'Insta']), df[,'City'], 'False')) %>%
      filter(Outlier!='False') %>%
      select(City,Insta) %>%
      arrange(desc(Insta))
  })
  
  
  
  output$Facet1 <- renderPlot({
    
    box_title = x_axis %>%
      select(Wifi) %>%
      filter(row_number()==1)
    
    x_long = x_axis %>%
      select(Wifi) %>%
      filter(row_number()==2)
    
    ggplot(df, aes(x=Wifi)) + 
      geom_boxplot(color="blue", outlier.color = "blue", outlier.size = 1) +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      ggtitle(box_title) +
      theme(plot.title=element_text(hjust=0.5, size=16, color='black')) +
      labs(x=x_long) + 
      theme(axis.title.x=element_text(size=12, color='black')) +
      theme(axis.text.x=element_text(size=9, color='blue')) + 
      scale_x_continuous(expand = c(0.13, 0))
    
    
  })
  
  output$Facet2 <- renderPlot({
    
    box_title = x_axis %>%
      select(Co_Work) %>%
      filter(row_number()==1)
    
      x_long = x_axis %>%
        select(Co_Work) %>%
        filter(row_number()==2)
    
    ggplot(df, aes(x=Co_Work)) + 
      geom_boxplot(color="blue", outlier.color = "blue", outlier.size = 1) +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      ggtitle(box_title) +
      theme(plot.title=element_text(hjust=0.5, size=16, color='black')) +
      labs(x=x_long) + 
      theme(axis.title.x=element_text(size=12, color='black')) +
      theme(axis.text.x=element_text(size=9, color='blue')) + 
      scale_x_continuous(expand = c(0.13, 0))
    
    
  })
  
  output$Facet3 <- renderPlot({
    
    box_title = x_axis %>%
      select(Coffee) %>%
      filter(row_number()==1)
    
    x_long = x_axis %>%
      select(Coffee) %>%
      filter(row_number()==2)
    
    ggplot(df, aes(x=Coffee)) + 
      geom_boxplot(color="blue", outlier.color = "blue", outlier.size = 1) +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      ggtitle(box_title) +
      theme(plot.title=element_text(hjust=0.5, size=16, color='black')) +
      labs(x=x_long) + 
      theme(axis.title.x=element_text(size=12, color='black')) +
      theme(axis.text.x=element_text(size=9, color='blue')) + 
      scale_x_continuous(expand = c(0.13, 0))
    
    
  })
  
  output$Facet4 <- renderPlot({
    
    box_title = x_axis %>%
      select(Taxi) %>%
      filter(row_number()==1)
    
    x_long = x_axis %>%
      select(Taxi) %>%
      filter(row_number()==2)
    
    ggplot(df, aes(x=Taxi)) + 
      geom_boxplot(color="blue", outlier.color = "blue", outlier.size = 1) +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      ggtitle(box_title) +
      theme(plot.title=element_text(hjust=0.5, size=16, color='black')) +
      labs(x=x_long) + 
      theme(axis.title.x=element_text(size=12, color='black')) +
      theme(axis.text.x=element_text(size=9, color='blue')) + 
      scale_x_continuous(expand = c(0.13, 0))
    
    
  })
  
  output$Facet5 <- renderPlot({
    
    box_title = x_axis %>%
      select(Beer) %>%
      filter(row_number()==1)
    
    x_long = x_axis %>%
      select(Beer) %>%
      filter(row_number()==2)
    
    ggplot(df, aes(x=Beer)) + 
      geom_boxplot(color="blue", outlier.color = "blue", outlier.size = 1) +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      ggtitle(box_title) +
      theme(plot.title=element_text(hjust=0.5, size=16, color='black')) +
      labs(x=x_long) + 
      theme(axis.title.x=element_text(size=12, color='black')) +
      theme(axis.text.x=element_text(size=9, color='blue')) + 
      scale_x_continuous(expand = c(0.13, 0))
    
    
  })
  
  output$Facet6 <- renderPlot({
    
    box_title = x_axis %>%
      select(Rent) %>%
      filter(row_number()==1)
    
    x_long = x_axis %>%
      select(Rent) %>%
      filter(row_number()==2)
    
    ggplot(df, aes(x=Rent)) + 
      geom_boxplot(color="blue", outlier.color = "blue", outlier.size = 1) +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      ggtitle(box_title) +
      theme(plot.title=element_text(hjust=0.5, size=16, color='black')) +
      labs(x=x_long) + 
      theme(axis.title.x=element_text(size=12, color='black')) +
      theme(axis.text.x=element_text(size=9, color='blue')) + 
      scale_x_continuous(expand = c(0.13, 0))
    
    
  })
  
  output$Facet7 <- renderPlot({
    
    box_title = x_axis %>%
      select(Meal) %>%
      filter(row_number()==1)
    
    x_long = x_axis %>%
      select(Meal) %>%
      filter(row_number()==2)
    
    ggplot(df, aes(x=Meal)) + 
      geom_boxplot(color="blue", outlier.color = "blue", outlier.size = 1) +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      ggtitle(box_title) +
      theme(plot.title=element_text(hjust=0.5, size=16, color='black')) +
      labs(x=x_long) + 
      theme(axis.title.x=element_text(size=12, color='black')) +
      theme(axis.text.x=element_text(size=9, color='blue')) + 
      scale_x_continuous(expand = c(0.13, 0))
    
    
  })
  
  output$Facet8 <- renderPlot({
    
    box_title = x_axis %>%
      select(Sun) %>%
      filter(row_number()==1)
    
    x_long = x_axis %>%
      select(Sun) %>%
      filter(row_number()==2)
    
    ggplot(df, aes(x=Sun)) + 
      geom_boxplot(color="blue", outlier.color = "blue", outlier.size = 1) +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      ggtitle(box_title) +
      theme(plot.title=element_text(hjust=0.5, size=16, color='black')) +
      labs(x=x_long) + 
      theme(axis.title.x=element_text(size=12, color='black')) +
      theme(axis.text.x=element_text(size=9, color='blue')) + 
      scale_x_continuous(expand = c(0.13, 0))
    
    
  })
  
  output$Facet9 <- renderPlot({
    
    box_title = x_axis %>%
      select(Fun) %>%
      filter(row_number()==1)
    
    x_long = x_axis %>%
      select(Fun) %>%
      filter(row_number()==2)
    
    ggplot(df, aes(x=Fun)) + 
      geom_boxplot(color="blue", outlier.color = "blue", outlier.size = 1) +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      ggtitle(box_title) +
      theme(plot.title=element_text(hjust=0.5, size=16, color='black')) +
      labs(x=x_long) + 
      theme(axis.title.x=element_text(size=12, color='black')) +
      theme(axis.text.x=element_text(size=9, color='blue')) + 
      scale_x_continuous(expand = c(0.13, 0))
    
    
  })
  
  output$Facet10 <- renderPlot({
    
    box_title = x_axis %>%
      select(Insta) %>%
      filter(row_number()==1)
    
    x_long = x_axis %>%
      select(Insta) %>%
      filter(row_number()==2)
    
    ggplot(df, aes(x=Insta)) + 
      geom_boxplot(color="blue", outlier.color = "blue", outlier.size = 1) +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      ggtitle(box_title) +
      theme(plot.title=element_text(hjust=0.5, size=16, color='black')) +
      labs(x=x_long) + 
      theme(axis.title.x=element_text(size=12, color='black')) +
      theme(axis.text.x=element_text(size=9, color='blue')) + 
      scale_x_continuous(expand = c(0.13, 0))
    
    
  })
  
  
  
  
  
  
  output$Dist <- renderPlot({
    
    x = df %>%
      filter(df$City == input$Ci) %>%
      select(input$Para) %>%
      pull()
    
    ggplot(df, aes(df[,input$Para])) +
      geom_density() + 
      geom_vline(aes(xintercept=median(df[,input$Para]), color="Median"),
                 linetype='dashed', size=1.5) +
      geom_vline(aes(xintercept=x, color='City'), linetype='dashed', size=1.5) +
      annotate("label",x=median(df[,input$Para]), y=-Inf, label=median(df[,input$Para]), 
               size=7, vjust='inward') +
      annotate("label",x=x, y=Inf, label=paste0(input$Ci, " = ", as.character(x)), size=7, vjust='inward') +
      labs(title=x_axis[1,input$Para], x=x_axis[2,input$Para], y='Density') +
      scale_color_manual(name="Vertical Markers", values=c(Median='blue', City='red')) +
      theme(plot.title = element_text(hjust=0.5, size=24)) +
      theme(legend.text=element_text(size=19)) + 
      theme(legend.title=element_text(size=19)) +
      theme(axis.title.x = element_text(size=17)) +
      theme(axis.title.y = element_text(size=17)) + 
      theme(axis.text.x = element_text(size=13)) +
      scale_x_discrete(expand=c(0.09,0.09))
      #theme(plot.margin = margin(0,-0.5,0,0, "cm"))
    }
  )
  
  output$View <- renderTable({
    df %>%
      select(City, .data[[input$Para]]) %>%
      arrange(desc(.data[[input$Para]])) %>%
      head(input$Top)
  })
  
  output$View2 <- renderTable({
    df %>%
      select(City, .data[[input$Para]]) %>%
      arrange(.data[[input$Para]]) %>%
      head(input$Bot)
  })
  
  output$CityComps <- renderTable({
    citycomp = citycomp %>%
      select(Parameter, input$CityComp1, input$CityComp2) %>%
      mutate('Absolute Diff (City1 - City2)' = .data[[input$CityComp1]] - .data[[input$CityComp2]]) %>%
      mutate('Ratio (City1 / City2)' = (.data[[input$CityComp1]]/.data[[input$CityComp2]]))
    
  })

  output$Bar1 <- renderPlot({
    citycomp_bar = citycomp %>%
      select(Parameter, .data[[input$CityComp1]], .data[[input$CityComp2]]) %>%
      filter(row_number()==1) 
    
    citycomp_bar %>%
      pivot_longer(
        cols = c(input$CityComp1, input$CityComp2),
        names_to = "City",
        values_to = "Wifi_Speed_Mbps"
      ) %>%
      ggplot((aes(fill=City, y=Wifi_Speed_Mbps, x=City))) + 
      geom_bar(position='dodge', stat='identity') + 
      ggtitle(citycomp_bar$Parameter[1]) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15))
      
  })
    
  output$Bar2 <- renderPlot({
    citycomp_bar = citycomp %>%
      select(Parameter, .data[[input$CityComp1]], .data[[input$CityComp2]]) %>%
      filter(row_number()==2) 
    
    citycomp_bar %>%
      pivot_longer(
        cols = c(input$CityComp1, input$CityComp2),
        names_to = "City",
        values_to = "Number_Of_Spaces"
      ) %>%
      ggplot((aes(fill=City, y=Number_Of_Spaces, x=City))) + 
      geom_bar(position='dodge', stat='identity') + 
      ggtitle(citycomp_bar$Parameter[1]) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15))
  })
    
  output$Bar3 <- renderPlot({
    citycomp_bar = citycomp %>%
      select(Parameter, .data[[input$CityComp1]], .data[[input$CityComp2]]) %>%
      filter(row_number()==3) 
    
    citycomp_bar %>%
      pivot_longer(
        cols = c(input$CityComp1, input$CityComp2),
        names_to = "City",
        values_to = "Price_Per_Cup_USD"
      ) %>%
      ggplot((aes(fill=City, y=Price_Per_Cup_USD, x=City))) + 
      geom_bar(position='dodge', stat='identity') + 
      ggtitle(citycomp_bar$Parameter[1]) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15)) 
    
  })
  
  output$Bar4 <- renderPlot({
    citycomp_bar = citycomp %>%
      select(Parameter, .data[[input$CityComp1]], .data[[input$CityComp2]]) %>%
      filter(row_number()==4) 
    
    citycomp_bar %>%
      pivot_longer(
        cols = c(input$CityComp1, input$CityComp2),
        names_to = "City",
        values_to = "Fare_Per_Km_USD"
      ) %>%
      ggplot((aes(fill=City, y=Fare_Per_Km_USD, x=City))) + 
      geom_bar(position='dodge', stat='identity') + 
      ggtitle(citycomp_bar$Parameter[1]) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15)) 
    
  })
  
  output$Bar5 <- renderPlot({
    citycomp_bar = citycomp %>%
      select(Parameter, .data[[input$CityComp1]], .data[[input$CityComp2]]) %>%
      filter(row_number()==5) 
    
    citycomp_bar %>%
      pivot_longer(
        cols = c(input$CityComp1, input$CityComp2),
        names_to = "City",
        values_to = "Price_Per_Glass_USD"
      ) %>%
      ggplot((aes(fill=City, y=Price_Per_Glass_USD, x=City))) + 
      geom_bar(position='dodge', stat='identity') + 
      ggtitle(citycomp_bar$Parameter[1]) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15)) 
    
  })
  
  output$Bar6 <- renderPlot({
    citycomp_bar = citycomp %>%
      select(Parameter, .data[[input$CityComp1]], .data[[input$CityComp2]]) %>%
      filter(row_number()==6) 
    
    citycomp_bar %>%
      pivot_longer(
        cols = c(input$CityComp1, input$CityComp2),
        names_to = "City",
        values_to = "Monthly_Rental_USD"
      ) %>%
      ggplot((aes(fill=City, y=Monthly_Rental_USD, x=City))) + 
      geom_bar(position='dodge', stat='identity') + 
      ggtitle(citycomp_bar$Parameter[1]) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15)) 
    
  })
  
  output$Bar7 <- renderPlot({
    citycomp_bar = citycomp %>%
      select(Parameter, .data[[input$CityComp1]], .data[[input$CityComp2]]) %>%
      filter(row_number()==7) 
    
    citycomp_bar %>%
      pivot_longer(
        cols = c(input$CityComp1, input$CityComp2),
        names_to = "City",
        values_to = "Price_Per_Meal_USD"
      ) %>%
      ggplot((aes(fill=City, y=Price_Per_Meal_USD, x=City))) + 
      geom_bar(position='dodge', stat='identity') + 
      ggtitle(citycomp_bar$Parameter[1]) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15)) 
    
  })
  
  output$Bar8 <- renderPlot({
    citycomp_bar = citycomp %>%
      select(Parameter, .data[[input$CityComp1]], .data[[input$CityComp2]]) %>%
      filter(row_number()==8) 
    
    citycomp_bar %>%
      pivot_longer(
        cols = c(input$CityComp1, input$CityComp2),
        names_to = "City",
        values_to = "Annual_Hours"
      ) %>%
      ggplot((aes(fill=City, y=Annual_Hours, x=City))) + 
      geom_bar(position='dodge', stat='identity') + 
      ggtitle(citycomp_bar$Parameter[1]) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15)) 
    
  })
  
  output$Bar9 <- renderPlot({
    citycomp_bar = citycomp %>%
      select(Parameter, .data[[input$CityComp1]], .data[[input$CityComp2]]) %>%
      filter(row_number()==9) 
    
    citycomp_bar %>%
      pivot_longer(
        cols = c(input$CityComp1, input$CityComp2),
        names_to = "City",
        values_to = "Number_Of_Attractions"
      ) %>%
      ggplot((aes(fill=City, y=Number_Of_Attractions, x=City))) + 
      geom_bar(position='dodge', stat='identity') + 
      ggtitle(citycomp_bar$Parameter[1]) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15)) 
    
  })
  
  output$Bar10 <- renderPlot({
    citycomp_bar = citycomp %>%
      select(Parameter, .data[[input$CityComp1]], .data[[input$CityComp2]]) %>%
      filter(row_number()==10) 
    
    citycomp_bar %>%
      pivot_longer(
        cols = c(input$CityComp1, input$CityComp2),
        names_to = "City",
        values_to = "Number_Of_Hashtags_in_Millions"
      ) %>%
      ggplot((aes(fill=City, y=Number_Of_Hashtags_in_Millions, x=City))) + 
      geom_bar(position='dodge', stat='identity') + 
      ggtitle(citycomp_bar$Parameter[1]) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size=15),
            legend.text = element_text(size=15)) 
    
  })
  
  output$Box <- renderPlot({
    
    P = scaled_longer %>%
      filter(City==input$CityBox1) %>%
      pull(Parameter)
    
    Z1 = scaled_longer %>%
      filter(City==input$CityBox1) %>%
      pull(Z_Scores)
    
    Z2 = scaled_longer %>%
      filter(City==input$CityBox2) %>%
      pull(Z_Scores)
    
    g = ggplot(scaled_longer, aes(Parameter, Z_Scores)) + geom_boxplot(outlier.color = 'black', outlier.shape = 1)
    #g
    
    g2 = g + geom_point(data=data.frame(x=factor(c(P)), y=c(Z1)),
                        aes(x=x, y=y, col="red1"),
                        size=4, show.legend = TRUE) + 
             scale_color_manual(values = c("red1" = "red1"), label = c(red1 = input$CityBox1)) + 
             theme(legend.text = element_text(size=30))
    
    #g2
    
    g3 = g2 + geom_point(data=data.frame(x=factor(c(P)), y=c(Z2)),
                         aes(x=x, y=y, col="blue"),
                         size=4, show.legend = TRUE) + 
      scale_color_manual(values = c("red1"="red1", "blue"="blue"), label = c(red1 = input$CityBox1, blue = input$CityBox2)) + 
      theme(legend.text = element_text(size=25)) + 
      theme(legend.title = element_text(size=30)) + 
      labs(color = "City")
    
    g3
    
  })
  
  output$Index_Ranking <- renderTable({
    
    P1 = index %>%
      select(input$ParaIndex1)*10
    
    P2 = index %>%
      select(input$ParaIndex2)*9
    
    P3 = index %>%
      select(input$ParaIndex3)*8
    
    P4 = index %>%
      select(input$ParaIndex4)*7
    
    P5 = index %>%
      select(input$ParaIndex5)*6
    
    P6 = index %>%
      select(input$ParaIndex6)*5
    
    P7 = index %>%
      select(input$ParaIndex7)*4
    
    P8 = index %>%
      select(input$ParaIndex8)*3
    
    P9 = index %>%
      select(input$ParaIndex9)*2
    
    P10 = index %>%
      select(input$ParaIndex10)*1
    
    wtd_ind = cbind(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
    
   Your_Ind = wtd_ind %>%
     mutate(index_value = rowSums(.)) %>%
     pull(index_value)
   
   Your_Index = round(Your_Ind, digits=2)
   
   ctry_rank = cbind(index$City, Your_Index)
   colnames(ctry_rank)[1] = 'Your_City'
   
   ctry_rank[order(desc(Your_Index)),c(1,2)] %>%
     head(input$Rankers)
   
  })  
  
})

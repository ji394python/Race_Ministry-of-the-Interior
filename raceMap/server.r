
shinyServer(function(input, output) {
  safe = read.csv('data.csv',fileEncoding="big-5")
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = safe$type,
    library = 'ion',
    markerColor = safe$type
  )
  pal = colorFactor(palette = c("orange", "green", "black","blue","yellow"), domain = safe$大類別)
  
  
  output$second <- renderUI({
    if (is.null(input$person)) return()
    if (input$person == 'NULL') return(NULL)
    if (input$person == '單身貴族') return(selectInput("sexual","性別",c("男","女")))
    if (input$person == '新婚夫妻') return(selectInput("sexual","性別",c("有小孩","無小孩")))
  })
  
  output$city_second <- renderUI({
    if (is.null(input$city)) return()
    if (input$city == 'NULL') return(NULL)
    if (input$city == '新北') return(selectInput("region","行政區",c("NULL","石碇","汐止","瑞芳","雙溪")))
  })
  
  output$city_third <- renderUI({
    if (input$region == 'NULL') return(NULL)
    if (input$region == '汐止') return(selectInput("small","里",c("NULL","自強里","興昌里")))
  })
  output$city_four <- renderUI({
    if (input$small == 'NULL') return(NULL)
    if (input$small == '興昌里') return(selectInput("road","街道",c("NULL","A街","B路")))
  })
  
  output$my_ui_title<-renderUI({
    if (forest()==TRUE){
      if(input$small=='興昌里')
        img(src='img.jpg', height = '180px')
    } else{
      return(NULL)
    }
  })
  output$my_ui_score<-renderUI({
    if (forest() == TRUE) {
      if(input$small=='興昌里')
        good = TRUE
        img(src='score.jpg', height = '180px',width='240px')
    }else{
      return(NULL)
    }
  })
  
  output$text<-renderText({
    if (forest() == TRUE) {
      if(input$small=='興昌里')
      "結果：良居！"
    }else{
      return(NULL)
    }
  })
  
  forest <- reactive({
    input$go.forest
    isolate({
      if (input$road!="NULL") {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
  })
  mymap_proxy <- leafletProxy("mymap")
  
  observeEvent(input$go.forest,{
    mymap_proxy %>%
      flyTo(lng = 121.663, lat = 25.068, zoom = 16.5) %>%
      addPolygons(data = safe, 
                  lng = c(121.653699,121.658102,121.658425,121.65795,121.659098,121.659027, 121.657161,121.654619)
                  , lat = c(25.062448,25.06434,25.063514,25.063027,25.061911,25.060813,25.060093,25.060402),
                  fill = TRUE, weight = 2, color = "#orange", group = "Outline"
                  ,options = list(zIndex = 100)) %>%
      addPolygons(data = safe, 
                  lng = c(121.661137,121.662139,121.661598,121.661911,121.666066,121.6656, 121.665804,121.663959)
                  , lat = c(25.067812,25.066705,25.066087,25.065834,25.067343,25.068474,25.06984,25.070422),
                  fill = TRUE, weight = 2, color = "#orange", group = "Outline"
                  ,options = list(zIndex = 100))
  })

  
  output$mymap <- renderLeaflet({
    # mapping the quakes
    mymap = leaflet(data=safe) %>% # initialize the map ad dataset to be used
      
      addTiles(group = "OSM (default)") %>%
      addProviderTiles("Stamen.Toner", group = "Toner") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite")
    
    
    # Below code will add circle markers and groups based on types.
    # Create groups within the addCirleMarkers. Subsetted data is used for individual groups
    for(t in unique(safe$大類別)){ # loop through each of the types we defined earlier
      sub = safe[safe$大類別==t,] # subset the data based on each type
      mymap=mymap %>% 
        # add Circlemarker
        addCircleMarkers(
          data=sub,
          lng =  ~wgs84X,
          lat =  ~wgs84Y,
          radius = 3.5,
          color =  ~type,
          stroke = FALSE, 
          fillOpacity = 1,
          label= sub$Name,
          group = t,
          options = list(zIndex = 300)
        ) 
      
    }
    
    
    # Below code to add control layers and legend to the map.
    mymap %>% 
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = unique(safe$大類別),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      
      
      # add legend to the map
      addLegend(
        position = "bottomright",
        pal = pal,
        values = safe$大類別,
        title = "Type based on magnitude",
        opacity = 1
      )
    
  })
}
)
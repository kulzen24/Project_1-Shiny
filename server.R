library(shinydashboard)
library(DT)
library(googleVis)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(RColorBrewer)

shinyServer(function(input, output, session){
  
  #Interactive Map Output
  #To make these dynamic, I should set the leaflet call itself to be a variable
  output$la_map <- renderLeaflet({
    leaflet(dodger) %>%
      addTiles() %>% 
      addPolygons(popup = ~name,
                  fillColor = switch(input$selected,
                                     'MedHHIncome' = ~colIncome(dodger$MedHHIncome),
                                     'AveHouseholdSize' = ~colHHSize(dodger$AveHouseholdSize),
                                     'ViolentCrime' = ~colVio(dodger$ViolentCrime),
                                     'PropertyCrime' = ~colProp(dodger$PropertyCrime),
                                     'MedEducation' = ~colEd(dodger$MedEducation),
                                     'MedAge' = ~colAge(dodger$MedAge),
                                     'PctWhite' = ~colWhite(dodger$PctWhite),
                                     'PctBlack' = ~colBlack(dodger$PctBlack),
                                     'PctAsian' = ~colAsian(dodger$PctAsian),
                                     'PctHispanic' = ~colHispanic(dodger$PctHispanic),
                                     'PctForeignBorn' = ~colForeign(dodger$PctForeignBorn),
                                     'WalkScore' = ~colWalk(dodger$WalkScore),
                                     'TransitScore' = ~colTrans(dodger$TransitScore),
                                     'BikeScore' = ~colBike(dodger$BikeScore)
                                     ),
                  label = ~paste0(dodger$name, ": ", formatC(dodger[[input$selected]], big.mark = ",")),
                  smoothFactor = 0.3,
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) %>% #This column will contain tale of the tape + WalkScore
      addProviderTiles("CartoDB.Positron") %>% setView(-118.38, 34.07, zoom = 10) %>%
      addLegend(pal = colIncome, values = dodger$MedHHIncome, 
                opacity = 0.7, title = NULL,
                position = "bottomright")
  })
  #ifelses need to be fillColor in Polygons and values in Legend
  
  output$ny_map <- renderLeaflet({
    leaflet(yankee) %>%
      addTiles() %>% 
      addPolygons(popup = ~neighborhood,
                  fillColor = switch(input$selected,
                                     'MedHHIncome' = ~NYcolIncome(yankee$MedHHIncome),
                                     'AveHouseholdSize' = ~NYcolHHSize(yankee$AveHouseholdSize),
                                     'ViolentCrime' = ~NYcolVio(yankee$ViolentCrime),
                                     'PropertyCrime' = ~NYcolProp(yankee$PropertyCrime),
                                     'MedEducation' = ~NYcolEd(yankee$MedEducation),
                                     'MedAge' = ~NYcolAge(yankee$MedAge),
                                     'PctWhite' = ~NYcolWhite(yankee$PctWhite),
                                     'PctBlack' = ~NYcolBlack(yankee$PctBlack),
                                     'PctAsian' = ~NYcolAsian(yankee$PctAsian),
                                     'PctHispanic' = ~NYcolHispanic(yankee$PctHispanic),
                                     'PctForeignBorn' = ~NYcolForeign(yankee$PctForeignBorn),
                                     'WalkScore' = ~NYcolWalk(yankee$WalkScore),
                                     'TransitScore' = ~NYcolTrans(yankee$TransitScore),
                                     'BikeScore' = ~NYcolBike(yankee$BikeScore)
                  ),
                  label = ~paste0(yankee$neighborhood, ": ", formatC(yankee[[input$selected]], big.mark = ",")),
                  smoothFactor = 0.3,
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) %>% #This column will contain tale of the tape + WalkScore
      addProviderTiles("CartoDB.Positron") %>% setView(-73.89, 40.67, zoom = 11) %>%
      addLegend(pal = colIncome, values = dodger$MedHHIncome, 
                opacity = 0.7, title = NULL,
                position = "bottomright")
  })
  
  #collapse Maps
  # observeEvent(input$bt1, {
  #   js$collapse("box1")
  # })
  # observeEvent(input$bt2, {
  #   js$collapse("box2")
  # })
  
  # show statistics using infoBox

  output$maxBox1 <- renderInfoBox({
    max_value <- max(lafax[[input$selected]], na.rm = TRUE)
    max_hood <-
      lafax$NhoodName[lafax[[input$selected]]==max_value]
    infoBox(max_hood, formatC(max_value, big.mark = ","), icon = icon("hand-o-up"))
  })

  output$minBox1 <- renderInfoBox({
    min_value <- min(lafax[[input$selected]], na.rm =TRUE)
    min_hood <-
      lafax$NhoodName[lafax[[input$selected]]==min_value]
    infoBox(min_hood, formatC(min_value, big.mark = ","), icon = icon("hand-o-down"))
  })

  output$medBox1 <- renderInfoBox(
    infoBox(paste(formatC("Median", input$selected, big.mark = ",")),
            median(lafax[[input$selected]], na.rm = TRUE),
            icon = icon("calculator"), fill = TRUE))
  
  output$maxBox2 <- renderInfoBox({
    max_value <- max(nyfax[[input$selected]], na.rm = TRUE)
    max_hood <-
      nyfax$NhoodName[nyfax[[input$selected]]==max_value]
    infoBox(max_hood, formatC(max_value, big.mark = ","), icon = icon("hand-o-up"))
  })
  
  output$minBox2 <- renderInfoBox({
    min_value <- min(nyfax[[input$selected]], na.rm =TRUE)
    min_hood <-
      nyfax$NhoodName[nyfax[[input$selected]]==min_value]
    infoBox(min_hood, formatC(min_value, big.mark = ","), icon = icon("hand-o-down"))
  })
  
  output$medBox2 <- renderInfoBox(
    infoBox(paste(formatC("Median", input$selected, big.mark = ",")),
            median(nyfax[[input$selected]], na.rm = TRUE),
            icon = icon("calculator"), fill = TRUE))
  
  #Interactive Plots and Graphs in Insights tab
  
  #Median Household Income by Education Level
  output$income_ed <- renderGvis({
    gvisCandlestickChart(candlequants1, xvar = 'MedEducation', low = 'low',
                         open = 'open', close = 'close', 
                         high = 'high', options=cf1_ops)
  })
  
  
  #Violent Crime Rate vs Property Crime Rate
  output$vio_prop <- renderGvis({
    gvisScatterChart(scatterfax1,options=sf1_ops)
  })
  
  #Household Size as Function of Income
  output$size_income <- renderGvis({
    gvisScatterChart(scatterfax2,options=sf2_ops)
  })
  
  #Violent Crime Histogram
  output$histovio <- renderGvis({
    gvisHistogram(histofax1, options=h1_ops)
  })
  
  #Property Crime Histogram
  output$histoprop <- renderGvis({
    gvisHistogram(histofax2, options=h2_ops)
  })
  
  #Median Age Histogram
  output$histo_age <- renderGvis({
    gvisHistogram(histofax3, options=h3_ops)
  })  
  
  #Median Education Histogram
  output$histo_ed <- renderGvis({
    gvisHistogram(histofax4, options=h4_ops)
  })
  
  # #Transit Score Scatter Plot
  # output$transitscores <- renderGvis({
  #   gvisScatterChart(scatterfax3,options=sf3_ops)
  # })
  
  #Show Data Table
  output$table <- DT::renderDataTable({
    datatable(fax_b, rownames=FALSE) %>%
    formatStyle(input$selectedjr,
                background="skyblue", fontWeight='bold')
  })
  
  #Match-maker
  observeEvent(input$city1, {
    choices = unique(fax_b[CityID == input$city1, NhoodName])
    updateSelectizeInput(session, inputId = 'hood', choices = choices)
  })
  
  output$basehood <- renderValueBox({
    basehood_val <- fax_b[NhoodName == input$hood, NhoodName]
    valueBox(basehood_val, subtitle = "What's similar?", icon = icon("building-o"))
  })


  output$match1 <- renderValueBox({
    getonehood = if (input$city1 == 'Los Angeles') {lafax %>% filter(., NhoodName == input$hood) %>% select(., feature)} else{
      nyfax %>% filter(., NhoodName == input$hood) %>% select(., feature)
    }
    if (input$city2 == 'Los Angeles'){myset = lafax} else if (input$city2 == 'New York') {myset = nyfax}
    mymatches = matchmaker(myset[, input$compare_on], getonehood[, input$compare_on])
    hood <- mymatches[1]
    first = myset$NhoodName[hood]
    valueBox(tags$p(first, style = 'font-size: 50%;'), paste('The most similar neighborhood in', input$city2, 'compared with', input$hood, 'regarding', input$compare_on), icon = icon("question-circle-o"))
  })
  
  output$match2 <- renderValueBox({
    getonehood = if (input$city1 == 'Los Angeles') {lafax %>% filter(., NhoodName == input$hood) %>% select(., feature)} else{
      nyfax %>% filter(., NhoodName == input$hood) %>% select(., feature)
    }
    if (input$city2 == 'Los Angeles'){myset = lafax} else if (input$city2 == 'New York') {myset = nyfax}
    mymatches = matchmaker(myset[, input$compare_on], getonehood[, input$compare_on])
    hood <- mymatches[2]
    second = myset$NhoodName[hood]
    valueBox(tags$p(second, style = 'font-size: 50%;'), paste('The 2nd most similar neighborhood in', input$city2, 'compared with', input$hood, 'regarding', input$compare_on), icon = icon("question-circle-o"))
  })
  
  output$match3 <- renderValueBox({
    getonehood = if (input$city1 == 'Los Angeles') {lafax %>% filter(., NhoodName == input$hood) %>% select(., feature)} else{
      nyfax %>% filter(., NhoodName == input$hood) %>% select(., feature)
    }
    if (input$city2 == 'Los Angeles'){myset = lafax} else if (input$city2 == 'New York') {myset = nyfax}
    mymatches = matchmaker(myset[, input$compare_on], getonehood[, input$compare_on])
    hood <- mymatches[3]
    third = myset$NhoodName[hood]
    valueBox(tags$p(third, style = 'font-size: 50%;'), paste('The 3rd most similar neighborhood in', input$city2, 'compared with', input$hood, 'regarding', input$compare_on), icon = icon("question-circle-o"))
  })
  
  
  # output$matches <- DT::renderDataTable({
  #   getonehood = if (input$city1 == 'Los Angeles') {lafax %>% filter(., NhoodName == input$hood) %>% select(., feature)} else{
  #     nyfax %>% filter(., NhoodName == input$hood) %>% select(., feature)
  #   }
  #   if (input$city2 == 'Los Angeles'){myset = lafax} else if (input$city2 == 'New York') {myset = nyfax}
  #   mymatches = matchmaker(myset[, input$compare_on], getonehood[, input$compare_on])
  #   hood <- mymatches[1]
  #   first = myset$NhoodName[hood]
  #   print(first)
  #   hood <- mymatches[2]
  #   second = myset$NhoodName[hood]
  #   print(second)
  #   hood <- mymatches[3]
  #   third = myset$NhoodName[hood]
  #   print(third)
  #   myset = myset %>% filter(., NhoodName %in% c(first, second, third)) %>% select(., NhoodName, tablestats)
  #   print(myset)
  #   getonehood = if (input$city1 == 'Los Angeles') {lafax %>% filter(., NhoodName == input$hood) %>% select(., NhoodName, tablestats)} else{
  #     nyfax %>% filter(., NhoodName == input$hood) %>% select(., NhoodName, tablestats)
  #   }
  #   mytable = rbind(getonehood, myset)
  #   print(mytable)
  #   datatable(mytable, rownames=FALSE) %>%
  #   formatStyle(input$hood,
  #               background="skyblue", fontWeight='bold')
  # })
})
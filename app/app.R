# Shiny app to estimate water use of cotton based on measurements at AZMet stations and by date range

# Add code for the following
# 
# 'azmet-shiny-template.html': <!-- Google tag (gtag.js) -->


# UI --------------------


ui <- htmltools::htmlTemplate(
  
  filename = "azmet-shiny-template.html",
  
  pageCottonWaterUseEstimates = bslib::page(
    title = NULL,
    theme = theme, # `scr##_theme.R`
    
    bslib::layout_sidebar(
      sidebar = sidebar, # `scr##_pageSidebar.R`
      shiny::htmlOutput(outputId = "navsetCardTabTitle"),
      shiny::htmlOutput(outputId = "navsetCardTabSummary"),
      shiny::uiOutput(outputId = "navsetCardTab")
    ) |>
      htmltools::tagAppendAttributes(
        #https://getbootstrap.com/docs/5.0/utilities/api/
        class = "border-0 rounded-0 shadow-none"
      ),
    
    shiny::htmlOutput(outputId = "downloadButtonsDiv"), # Common, regardless of card tab
    shiny::htmlOutput(outputId = "pageBottomText") # Common, regardless of card tab
  )
)


# Server --------------------


server <- function(input, output, session) {
  shinyjs::useShinyjs(html = TRUE)
  shinyjs::hideElement(id = "downloadButtonsDiv")
  shinyjs::hideElement(id = "navsetCardTab")
  
  
  # Observables -----
  
  shiny::observeEvent(waterUse(), {
    shinyjs::showElement(id = "downloadButtonsDiv")
    shinyjs::showElement(id = "navsetCardTab")
    showNavsetCardTab(TRUE)
    showPageBottomText(TRUE)
  })
  
  # To update available dates based on selected station
  shiny::observeEvent(input$azmetStation, {
    stationStartDate <-
      dplyr::filter(azmetStationMetadata, meta_station_name == input$azmetStation) %>% 
      dplyr::pull(start_date)
    
    stationStartDateMinimum <- stationStartDate
    stationEndDateMinimum <- stationStartDate
    
    if (stationStartDate > input$startDate) {
      stationStartDateSelected <- stationStartDate
    } else {
      stationStartDateSelected <- input$startDate
    }
    
    if (stationStartDate > input$endDate) {
      stationEndDateSelected <- stationStartDate
    } else {
      stationEndDateSelected <- input$endDate
    }
    
    shiny::updateDateInput(
      inputId = "startDate",
      label = "Planting Date",
      value = stationStartDateSelected,
      min = stationStartDateMinimum,
      max = Sys.Date() - 1
    )
    
    shiny::updateDateInput(
      inputId = "endDate",
      label = "End Date",
      value = stationEndDateSelected,
      min = stationEndDateMinimum,
      max = Sys.Date() - 1
    )
  })
  
  shiny::observeEvent(input$estimateWaterUse, {
    if (input$startDate > input$endDate) {
      shiny::showModal(datepickerErrorModal) # `scr##_datepickerErrorModal.R`
    } 
    
    if (input$azmetStation == "Yuma N.Gila" & lubridate::int_overlaps(int1 = yugNodataInterval, int2 = lubridate::interval(input$startDate, input$endDate)) == TRUE) {
      shiny::showModal(datepickerYumaNGilaErrorModal) # `scr##_datepickerYumaNGilaErrorModal.R`    input$azmetStation == "Yuma N.Gila" & 
    }
  })
  
  # shiny::observeEvent(input$estimateWaterUse, {
  #   if (input$azmetStation == "Yuma N.Gila" & input$startDate >= as.Date("2021-06-16") & input$startDate <= as.Date("2021-10-10") & input$endDate >= as.Date("2021-06-16") & input$endDate <= as.Date("2021-10-10")) {
  #     # shiny::showModal(datepickerErrorModal) # `scr##_datepickerErrorModal.R`
  #     shiny::showModal(datepickerYumaNGilaErrorModal) # `scr##_datepickerYumaNGilaErrorModal.R`
  #   }
  # })
   
    
    
    # startDate <- as.Date("2021-07-01")
    # endDate <- as.Date("2021-07-30")

    # if (azmetStation == "Yuma N.Gila" & startDate >= as.Date("2021-06-16") & startDate <= as.Date("2021-10-10") & endDate >= as.Date("2021-06-16") & endDate <= as.Date("2021-10-10")) {
    #   # shiny::showModal(datepickerErrorModal) # `scr##_datepickerErrorModal.R`
    #   print("datepickerYumaNGilaErrorModal") # `scr##_datepickerYumaNGilaErrorModal.R`
    # }
    # if (startDate >= as.Date("2021-06-16") & startDate <= as.Date("2021-10-10")) {
    #   if (endDate >= as.Date("2021-06-16") & endDate <= as.Date("2021-10-10")) {
    #     print("datepickerYumaNGilaErrorModal") # `scr##_datepickerYumaNGilaErrorModal.R`
    #   }
    # }


  
  # To update icon in navsetCardTab title
  shiny::observeEvent(input$navsetCardTab, {
    if (input$navsetCardTab == "barChart") {
      navsetCardTabTitleIcon("bar-chart-fill")
      # print("bar-chart-fill")
    } else if (input$navsetCardTab == "table") {
      navsetCardTabTitleIcon("table")
      # print("table")
    } else if (input$navsetCardTab == "timeSeries") {
      navsetCardTabTitleIcon("graph-up")
      # print("graph-up")
    }
  })
  
  
  # Reactives -----
  
  navsetCardBarChart <- shiny::eventReactive(waterUse(), {
    fxn_navsetCardBarChart(
      inData = waterUse()[[2]],
      azmetStation = input$azmetStation
    )
  })
  
  navsetCardBarChartCaption <- shiny::eventReactive(waterUse(), {
    fxn_navsetCardBarChartCaption(
      azmetStation = input$azmetStation,
      inData = waterUse()[[2]],
      startDate = input$startDate,
      endDate = input$endDate
    )
  })
  
  navsetCardTable <- shiny::eventReactive(waterUse(), {
    fxn_navsetCardTable(
      inData = waterUse()[[1]],
      startDate = input$startDate,
      endDate = input$endDate
    )
  })
  
  navsetCardTableCaption <- shiny::eventReactive(waterUse(), {
    fxn_navsetCardTableCaption()
  })
  
  navsetCardTabSummary <- shiny::eventReactive(waterUse(), {
    fxn_navsetCardTabSummary(
      azmetStation = input$azmetStation,
      inData = waterUse()[[2]],
      startDate = input$startDate,
      endDate = input$endDate
    )
  })
  
  navsetCardTabTitle <- shiny::eventReactive(list(navsetCardTabTitleIcon(), waterUse()), {
    fxn_navsetCardTabTitle(
      azmetStation = input$azmetStation,
      navsetCardTabTitleIcon = navsetCardTabTitleIcon()
    )
  })
  
  navsetCardTabTooltipText <- shiny::eventReactive(input$navsetCardTab, {
    fxn_navsetCardTabTooltipText(
      navsetCardTab = input$navsetCardTab
    )
  })
  
  navsetCardTimeSeries <- shiny::eventReactive(waterUse(), {
    fxn_navsetCardTimeSeries(
      inData = waterUse()[[1]],
      startDate = input$startDate,
      endDate = input$endDate
    )
  })
  
  navsetCardTimeSeriesCaption <- shiny::eventReactive(waterUse(), {
    fxn_navsetCardTimeSeriesCaption(
      azmetStation = input$azmetStation,
      inData = waterUse()[[1]],
      startDate = input$startDate,
      endDate = input$endDate
    )
  })
  
  pageBottomText <- shiny::eventReactive(waterUse(), {
    fxn_pageBottomText()
  })
  
  waterUse <- shiny::eventReactive(input$estimateWaterUse, {
    shiny::validate(
      shiny::need(
        expr = input$startDate <= input$endDate,
        message = FALSE # Failing validation test
      ),
      shiny::need(
        expr = 
          !(
            input$azmetStation == "Yuma N.Gila" &
              lubridate::int_overlaps(
                int1 = yugNodataInterval, 
                int2 = lubridate::interval(input$startDate, input$endDate)
              )
          ),
        message = FALSE
      )
      
      # if (input$azmetStation == "Yuma N.Gila" &
      #     lubridate::int_overlaps(
      #       int1 = yugNodataInterval,
      #       int2 = lubridate::interval(startDate, endDate)
      #     ) == TRUE
      #    ) {
      #   FALSE # Failing validation test
      # } else {
      #   NULL # Passing validation test
      # }
      # if (input$startDate %within% yugNoRequestInterval | input$endDate %within% yugNoRequestInterval & 
      #     input$azmetStation == "Yuma N.Gila") {
      #   FALSE # Failing test
      # } else {
      #   NULL # Passing test
      # }
    )
    
    # if (input$azmetStation == "Yuma N.Gila" &
    #     lubridate::int_overlaps(
    #       int1 = yugNodataInterval,
    #       int2 = lubridate::interval(input$startDate, input$endDate)
    #     ) == TRUE
    # ) {
    #   shiny::validate(FALSE) # Failing validation test
    # }
    
    idEstimateWaterUse <- shiny::showNotification(
      ui = "Estimating cotton water use . . .",
      action = NULL,
      duration = NULL,
      closeButton = FALSE,
      id = "idEstimateWaterUse",
      type = "message"
    )
    
    on.exit(
      shiny::removeNotification(id = idEstimateWaterUse),
      add = TRUE
    )
    
    fxn_waterUse( # calls `fxn_azDaily.R`, `fxn_waterUseSeasonalTotal.R`
      azmetStation = input$azmetStation,
      startDate = input$startDate,
      endDate = input$endDate
    )
  })
  
  
  # Outputs -----
  
  output$downloadButtonsDiv <- shiny::renderUI({
    fxn_downloadButtonsDiv()
  })
  
  output$downloadCSV <- shiny::downloadHandler(
    filename = function() {"AZMet-cotton-water-use-estimates.csv"},
    content = function(file) {
      vroom::vroom_write(x = waterUse()[[1]], file = file, delim = ",")
    }
  )
  
  output$downloadTSV <- shiny::downloadHandler(
    filename = function() {"AZMet-cotton-water-use-estimates.tsv"},
    content = function(file) {
      vroom::vroom_write(x = waterUse()[[1]], file = file, delim = "\t")
    }
  )
  
  output$navsetCardBarChart <- plotly::renderPlotly({
    navsetCardBarChart()
  })
  
  output$navsetCardBarChartCaption <- shiny::renderUI({
    navsetCardBarChartCaption()
  })
  
  # Having `navsetCardTab` as `output` helps with hiding the tabs on app start up
  output$navsetCardTab <- shiny::renderUI({
    shiny::req(showNavsetCardTab())
    navsetCardTab # `scr##_navsetCardTab.R`
  })
  
  output$navsetCardTable <- reactable::renderReactable({
    navsetCardTable()
  })
  
  output$navsetCardTableCaption <- shiny::renderUI({
    navsetCardTableCaption()
  })
  
  output$navsetCardTabSummary <- shiny::renderUI({
    navsetCardTabSummary()
  })
  
  output$navsetCardTabTitle <- shiny::renderUI({
    navsetCardTabTitle()
  })
  
  output$navsetCardTabTooltip <- shiny::renderUI({
    navsetCardTabTooltipText()
  })
  
  output$navsetCardTimeSeries <- plotly::renderPlotly({
    navsetCardTimeSeries()
  })
  
  output$navsetCardTimeSeriesCaption <- shiny::renderUI({
    navsetCardTimeSeriesCaption()
  })
  
  output$pageBottomText <- shiny::renderUI({
    shiny::req(showPageBottomText())
    pageBottomText()
  })
}


# Run --------------------


shiny::shinyApp(ui = ui, server = server)

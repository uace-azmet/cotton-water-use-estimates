# Shiny app to estimate water use of cotton based on measurements at AZMet stations and by date range

# Add code for the following
# 
# 'azmet-shiny-template.html': <!-- Google tag (gtag.js) -->
# 'azmet-shiny-template.html': <!-- CSS specific to this AZMet Shiny app -->


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
  
  shiny::observeEvent(totalEvapotranspiration(), {
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
    
    # if (stationStartDate > initialStartDate) {
    #   stationStartDateMinimum <- stationStartDate
    #   stationEndDateMinimum <- stationStartDate
    # } else {
    #   stationStartDateMinimum <- Sys.Date() - lubridate::years(1)
    #   stationEndDateMinimum <- Sys.Date() - lubridate::years(1)
    # }
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
  })
  
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
  
  navsetCardBarChart <- shiny::eventReactive(totalEvapotranspiration(), {
    fxn_navsetCardBarChart(
      inData = totalEvapotranspiration()[[2]],
      azmetStation = input$azmetStation
    )
  })
  
  navsetCardBarChartCaption <- shiny::eventReactive(totalEvapotranspiration(), {
    fxn_navsetCardBarChartCaption(
      azmetStation = input$azmetStation,
      inData = totalEvapotranspiration()[[2]],
      startDate = input$startDate,
      endDate = input$endDate,
      etEquation = input$etEquation
    )
  })
  
  navsetCardTable <- shiny::eventReactive(totalEvapotranspiration(), {
    fxn_navsetCardTable(
      inData = totalEvapotranspiration()[[1]],
      startDate = input$startDate,
      endDate = input$endDate,
      etEquation = input$etEquation
    )
  })
  
  navsetCardTableCaption <- shiny::eventReactive(totalEvapotranspiration(), {
    fxn_navsetCardTableCaption(
      etEquation = input$etEquation
    )
  })
  
  navsetCardTabSummary <- shiny::eventReactive(totalEvapotranspiration(), {
    fxn_navsetCardTabSummary(
      azmetStation = input$azmetStation,
      inData = totalEvapotranspiration()[[2]],
      startDate = input$startDate,
      endDate = input$endDate
    )
  })
  
  navsetCardTabTitle <- shiny::eventReactive(list(navsetCardTabTitleIcon(), totalEvapotranspiration()), {
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
  
  navsetCardTimeSeries <- shiny::eventReactive(totalEvapotranspiration(), {
    fxn_navsetCardTimeSeries(
      inData = totalEvapotranspiration()[[1]],
      startDate = input$startDate,
      endDate = input$endDate,
      etEquation = input$etEquation
    )
  })
  
  navsetCardTimeSeriesCaption <- shiny::eventReactive(totalEvapotranspiration(), {
    fxn_navsetCardTimeSeriesCaption(
      azmetStation = input$azmetStation,
      inData = totalEvapotranspiration()[[1]],
      startDate = input$startDate,
      endDate = input$endDate,
      etEquation = input$etEquation
    )
  })
  
  pageBottomText <- shiny::eventReactive(totalEvapotranspiration(), {
    fxn_pageBottomText(
      # startDate = input$startDate, 
      # endDate = input$endDate,
      # etEquation = input$etEquation
    )
  })
  
  totalEvapotranspiration <- shiny::eventReactive(input$estimateWaterUse, {
    shiny::validate(
      shiny::need(
        expr = input$startDate <= input$endDate,
        message = FALSE
      )
    )
    
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
    
    fxn_totalEvapotranspiration( # calls `fxn_azDaily.R`, `fxn_etTotal.R`
      azmetStation = input$azmetStation,
      startDate = input$startDate,
      endDate = input$endDate,
      etEquation = input$etEquation
    )
  })
  
  
  # Outputs -----
  
  output$downloadButtonsDiv <- shiny::renderUI({
    fxn_downloadButtonsDiv()
  })
  
  output$downloadCSV <- shiny::downloadHandler(
    filename = function() {"AZMet-cotton-water-use-estimates.csv"},
    content = function(file) {
      vroom::vroom_write(x = totalEvapotranspiration()[[1]], file = file, delim = ",")
    }
  )
  
  output$downloadTSV <- shiny::downloadHandler(
    filename = function() {"AZMet-cotton-water-use-estimates.tsv"},
    content = function(file) {
      vroom::vroom_write(x = totalEvapotranspiration()[[1]], file = file, delim = "\t")
    }
  )
  
  output$navsetCardBarChart <- plotly::renderPlotly({
    navsetCardBarChart()
  })
  
  output$navsetCardBarChartCaption <- shiny::renderUI({
    navsetCardBarChartCaption()
  })
  
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

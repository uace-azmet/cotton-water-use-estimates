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
    # lang = "en",
    
    bslib::layout_sidebar(
      sidebar = pageSidebar, # `scr##_pageSidebar.R`
      shiny::htmlOutput(outputId = "navsetCardTabTitle"),
      shiny::htmlOutput(outputId = "navsetCardTabSummary"),
      # navsetCardTab # `scr##_navsetCardTab.R`
      shiny::uiOutput(outputId = "navsetCardTab")
    ) |>
      htmltools::tagAppendAttributes(
        #https://getbootstrap.com/docs/5.0/utilities/api/
        class = "border-0 rounded-0 shadow-none"
      ),
    
    shiny::htmlOutput(outputId = "pageBottomText") # Common, regardless of card tab
  )
)


# Server --------------------


server <- function(input, output, session) {
  # shinyjs::useShinyjs(html = TRUE)
  # shinyjs::hideElement("pageBottomText")
  
  
  # Observables -----
  
  shiny::observeEvent(azDaily(), {
    showNavsetCardTab(TRUE)
    showPageBottomText(TRUE)
    # shinyjs::showElement("pageBottomText")
  })
  
  # To update available dates based on selected station
  shiny::observeEvent(input$azmetStation, {
    stationStartDate <-
      dplyr::filter(
        azmetStationMetadata,
        meta_station_name == input$azmetStation
      )$start_date
    
    if (stationStartDate > initialStartDate) {
      stationStartDateMinimum <- stationStartDate
      stationEndDateMinimum <- stationStartDate
    } else {
      stationStartDateMinimum <- 
        dplyr::filter(activeStations, meta_station_name == input$azmetStation)$start_date
      stationEndDateMinimum <- 
        dplyr::filter(activeStations, meta_station_name == input$azmetStation)$start_date
    }
    
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
  
  azDaily <- shiny::eventReactive(input$estimateWaterUse, {
    shiny::validate(
      shiny::need(
        expr = input$startDate <= input$endDate,
        message = FALSE
      )
    )
    
    idEstimateWaterUse <- shiny::showNotification(
      ui = "Estimating water use . . .",
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
    
    fxn_azDaily(
      azmetStation = input$azmetStation,
      startDate = input$startDate,
      endDate = input$endDate
    )
  })
  
  navsetCardTabSummary <- shiny::eventReactive(azDaily(), {
    fxn_navsetCardTabSummary(
      azmetStation = input$azmetStation,
      startDate = input$startDate,
      endDate = input$endDate
    )
  })
  
  navsetCardTabTitle <- shiny::eventReactive(list(azDaily(), navsetCardTabTitleIcon()), {
    fxn_navsetCardTabTitle(
      azmetStation = input$azmetStation,
      navsetCardTabTitleIcon = navsetCardTabTitleIcon()
    )
  })
  
  
  # Outputs -----
  
  output$navsetCardTab <- shiny::renderUI({
    shiny::req(showNavsetCardTab())
    navsetCardTab # `scr##_navsetCardTab.R`
  })
  
  output$navsetCardTabSummary <- shiny::renderUI({
    navsetCardTabSummary()
  })
  
  output$navsetCardTabTitle <- shiny::renderUI({
    navsetCardTabTitle()
  })
  
  output$pageBottomText <- shiny::renderUI({
    shiny::req(showPageBottomText())
    fxn_pageBottomText()
  })
}


# Run --------------------


shiny::shinyApp(ui = ui, server = server)

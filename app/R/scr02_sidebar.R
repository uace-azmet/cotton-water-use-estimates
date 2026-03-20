sidebar <- bslib::sidebar(
  width = 300,
  position = "left",
  open = list(desktop = "open", mobile = "always-above"),
  id = "sidebar",
  title = NULL,
  bg = "#FFFFFF",
  fg = "#191919",
  class = NULL,
  max_height_mobile = NULL,
  gap = NULL,
  padding = NULL,
  
  htmltools::p(
    bsicons::bs_icon("sliders", class = "bolder-icon"), 
    htmltools::HTML("&nbsp;"), 
    htmltools::HTML("<strong>DATA OPTIONS</strong>&nbsp;"),
    bslib::tooltip(
      bsicons::bs_icon("info-circle"),
      "Select an AZMet station, a planting date, and a date for the end of the period of interest. Then, click or tap 'ESTIMATE WATER USE'.",
      id = "infoDataOptions",
      placement = "right"
    ),
    
    class = "data-options-title"
  ),
  
  shiny::selectInput(
    inputId = "azmetStation", 
    label = "AZMet Station",
    choices = azmetStationMetadata[order(azmetStationMetadata$meta_station_name), ]$meta_station_name,
    selected = azmetStationMetadata[order(azmetStationMetadata$meta_station_name), ]$meta_station_name[1]
  ),
  
  shiny::selectInput(
    inputId = "etEquation",
    label = "Equation",
    choices = etEquations,
    selected = "Penman-Monteith"
  ),
  
  shiny::dateInput(
    inputId = "startDate",
    label = "Planting Date",
    value = Sys.Date() - lubridate::dweeks(x = 1),
    min = initialStartDateMinimum,
    max = Sys.Date() - 1,
    format = "MM d, yyyy",
    startview = "month",
    weekstart = 0, # Sunday
    width = "100%",
    autoclose = TRUE
  ),
  
  shiny::dateInput(
    inputId = "endDate",
    label = "End Date",
    value = Sys.Date() - 1,
    min = Sys.Date() - lubridate::years(1),
    max = Sys.Date() - 1,
    format = "MM d, yyyy",
    startview = "month",
    weekstart = 0, # Sunday
    width = "100%",
    autoclose = TRUE
  ),
  
  shiny::actionButton(
    inputId = "estimateWaterUse",
    label = "ESTIMATE WATER USE",
    class = "btn btn-block btn-blue"
  )   
) # bslib::sidebar()

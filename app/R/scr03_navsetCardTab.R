navsetCardTab <- bslib::navset_card_tab(
  id = "navsetCardTab",
  selected = "barChart",
  title = NULL,
  sidebar = NULL,
  header = NULL,
  footer = NULL,
  height = 400,
  # height = 700,
  full_screen = TRUE,
  # wrapper = card_body,
  
  bslib::nav_panel(
    title = "Bar Chart",
    value = "barChart"#,
    # shiny::htmlOutput(outputId = "reportingText")
  ),
  
  bslib::nav_panel(
    title = "Table",
    value = "table",#
    
    # bslib::layout_sidebar(
    #   sidebar = timeseriesSidebar, # `scr##_timeseriesSidebar.R`
    #   
    #   shiny::htmlOutput(outputId = "timeseriesGraphTitle"),
    #   plotly::plotlyOutput("timeseriesGraph"),
    #   shiny::htmlOutput(outputId = "timeseriesGraphFooter")
    # )
  ),
  
  bslib::nav_panel(
    title = "Time Series",
    value = "timeSeries",#
    # shiny::htmlOutput(outputId = "validationText")
  )
) |>
  htmltools::tagAppendAttributes(
    #https://getbootstrap.com/docs/5.0/utilities/api/
    class = "border-0 rounded-0 shadow-none"
  )

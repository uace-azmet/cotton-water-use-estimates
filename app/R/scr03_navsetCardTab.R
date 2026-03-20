navsetCardTab <- bslib::navset_card_tab(
  id = "navsetCardTab",
  selected = "barChart",
  title = NULL,
  sidebar = NULL,
  header = NULL,
  footer = NULL,
  height = 600,
  full_screen = TRUE,
  # wrapper = card_body,
  
  bslib::nav_panel(
    title = "Bar Chart",
    value = "barChart",
    
    plotly::plotlyOutput(outputId = "navsetCardBarChart"),
    shiny::htmlOutput(outputId = "navsetCardBarChartCaption")
  ),
  
  bslib::nav_panel(
    title = "Table",
    value = "table",
    
    reactable::reactableOutput(outputId = "navsetCardTable"),
    shiny::htmlOutput(outputId = "navsetCardTableCaption")
  ),
  
  bslib::nav_panel(
    title = "Time Series",
    value = "timeSeries",
    
    plotly::plotlyOutput(outputId = "navsetCardTimeSeries"),
    shiny::htmlOutput(outputId = "navsetCardTimeSeriesCaption")
  ),
  
  bslib::nav_item(
    shiny::uiOutput(outputId = "navsetCardTabTooltip")
  )
) |>
  htmltools::tagAppendAttributes(
    #https://getbootstrap.com/docs/5.0/utilities/api/
    class = "border-0 rounded-0 shadow-none"
  )

#' `fxn_navsetCardTabTooltipText.R` - Build tooltip text for navset card tab section based on selected tab
#' 
#' @param navsetCardTab - Currently selected tab
#' @return `navsetCardTabTooltipText` - Tooltip text for navset card tab section based on selected tab


fxn_navsetCardTabTooltipText <- function(navsetCardTab) {
  if (navsetCardTab == "barChart") {
    tooltipText <- "Hover over bars for values of total evapotranspiration."
  } else if (navsetCardTab == "table") {
    tooltipText <- "Scroll or swipe over the table to view additional rows and columns (when hidden)."
  } else if (navsetCardTab == "timeSeries") {
    tooltipText <- "Hover over data for variable values and click or tap on legend items to toggle data visibility."
  }
  
  navsetCardTabTooltipText <- 
    bslib::tooltip(
      bsicons::bs_icon("info-circle"),
      tooltipText,
      id = "navsetCardTabTooltipText",
      placement = "right"
    )
  
  return(navsetCardTabTooltipText)
}

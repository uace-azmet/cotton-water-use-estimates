#' `fxn_navsetCardTabSummary.R` - Build summary of estimated water use based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - Data table of seasonal water use estimates by year
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `navsetCardTabSummary` - Summary of estimated water use based on user inputs


fxn_navsetCardTabSummary <- function(azmetStation, startDate, endDate) {
  
  navsetCardTabSummary <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Estimated cotton water use at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", "x.x inches", "</b>. This is the same as the accumulation during this same month-day period in 2025, and the same as the station average."
        ),
      ),
      
      class = "navset-card-tab-summary"
    )
  
  return(navsetCardTabSummary)
}

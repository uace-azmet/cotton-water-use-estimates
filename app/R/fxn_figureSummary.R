#' `fxn_figureSummary.R` - Build summary of figure based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - Data table of seasonal chill accumulation by year
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param chillVariable - Chill variable selected by user
#' @return `figureSummary` - Summary of figure based on user inputs


fxn_figureSummary <- function(azmetStation, startDate, endDate) {
  
  figureSummary <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Estimated cotton water use at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", "x.x inches</b>. This is the same as the accumulation during this same month-day period in 2025, and the same as the station average."
        ),
      ),
      
      class = "figure-summary"
    )
  
  return(figureSummary)
}

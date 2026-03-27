#' `fxn_navsetCardTabSummary.R` - Build summary of estimated cotton water use value based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - data table of seasonal estimates of cotton water use by year
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `navsetCardTabSummary` - Summary of estimated cotton water use value based on user input


fxn_navsetCardTabSummary <- function(azmetStation, inData, startDate, endDate) {
  currentYear <- lubridate::year(endDate)
  currentYearTotal <- 
    dplyr::filter(inData, end_date_year == currentYear) %>% 
    dplyr::pull(water_use_seasonal_total)
  
  # For stations with only one year of data
  if (nrow(inData) == 1) {
    navsetCardTabSummary <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Estimated cotton water use at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", format(round(currentYearTotal, digits = 2), nsmall = 2), " inches</b>."
          ),
        ),
        
        class = "navset-card-tab-summary"
      )
  } else {
    averageTotal <- mean(inData$water_use_seasonal_total, na.rm = TRUE)
    previousYear <- currentYear - 1
    previousYearText <- 
      dplyr::filter(inData, end_date_year == previousYear) %>% 
      dplyr::pull(date_year_label)
    previousYearTotal <- 
      dplyr::filter(inData, end_date_year == previousYear) %>% 
      dplyr::pull(water_use_seasonal_total)
    differenceAverage <- currentYearTotal - averageTotal
    differencePreviousYear <- currentYearTotal - previousYearTotal
    
    if (round(differenceAverage, digits = 2) > 0) {
      differenceAverageText <- 
        paste0(
          format(abs(round(differenceAverage, digits = 2)), nsmall = 2), " inches above"
        )
    } else if (round(differenceAverage, digits = 2) < 0) {
      differenceAverageText <- 
        paste0(
          format(abs(round(differenceAverage, digits = 2)), nsmall = 2), " inches below"
        )
    } else { # if (differenceAverage = 0)
      differenceAverageText <- "equal to"
    }
    
    if (differencePreviousYear == 0.00) {
      differencePreviousYearText <- "the same as"
    } else if (differencePreviousYear > 0) {
      differencePreviousYearText <- 
        paste0(
          format(abs(round(differencePreviousYear, digits = 2)), nsmall = 2), " inches greater than"
        )
    } else { # if (differencePreviousYear < 0)
      differencePreviousYearText <- 
        paste0(
          format(abs(round(differencePreviousYear, digits = 2)), nsmall = 2), " inches less than"
        )
    }
    
    navsetCardTabSummary <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Estimated cotton water use at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", format(round(currentYearTotal, digits = 2), nsmall = 2), " inches</b>. This is ", differencePreviousYearText, " the total during this same month-day period in ", previousYearText, ", and ", differenceAverageText, " the station average."
          ),
        ),
        
        class = "navset-card-tab-summary"
      )
  }
  
  return(navsetCardTabSummary)
}

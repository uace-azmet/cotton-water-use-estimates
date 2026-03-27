#' `fxn_navsetCardBarChartCaption.R` - Build caption for bar chart based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - Data table [[2]] from `fxn_waterUse.R`
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `navsetCardBarChartCaption` Caption for bar chart based on selected station


fxn_navsetCardBarChartCaption <- function(azmetStation, inData, startDate, endDate) {
  
  azmetStationStartDate <- 
    dplyr::filter(azmetStationMetadata, meta_station_name == azmetStation) %>% 
    dplyr::pull(start_date)
  
  if (nrow(inData) == 1) {
    standardText <- 
      paste0(
        "Estimated cotton water use (black bar in graph) is based on the sum of daily totals during the period of interest. Data for the ", azmetStation, " station in the new AZMet database currently go back to ", gsub(" 0", " ", format(azmetStationStartDate, "%B %d, %Y")), "."
      )
  } else {
    standardText <- 
      paste0(
        "Estimated cotton water use for the current year (black bar in graph) is based on the sum of daily totals during the period of interest. Estmates for past years (gray bars in graph) are based on the same start and end month and day, but during those respective years. Average estimated cotton water use is calculated from values of all individual years shown above. Data for the ", azmetStation, " station in the new AZMet database currently go back to ", gsub(" 0", " ", format(azmetStationStartDate, "%B %d, %Y")), "."
      )
  }
  
  variableKeyText <- 
    "Variable key: <strong>WU<sub>cumulative</sub> (in)</strong> accumulation of daily water use estimates in inches"
  
  # Account for multi-month absence of YUG data in 2021
  nonOperational <- 0
  
  if (azmetStation == "Yuma N.Gila") {
    while (startDate >= azmetStationStartDate) {
      userDateRange <- lubridate::interval(start = startDate, end = endDate)

      if (lubridate::int_overlaps(int1 = yugNodataInterval, int2 = userDateRange) == TRUE) {
        nonOperational <- 1
      }

      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    }
  }
  
  # Generate figure footer based on presence/absence of non-operational dates
  if (azmetStation == "Yuma N.Gila" & nonOperational == 1) {
    navsetCardBarChartCaption <- 
      htmltools::p(
        htmltools::HTML(
          paste(
            standardText,
            "However, we do not show cotton water use estimates for the year with a month-day period that overlaps the period from June 16, 2021 through October 21, 2021, when the ", azmetStation, " station was not in operation.",
            variableKeyText,
            sep = " "
          )
        ),
        
        class = "navset-card-caption"
      )
  } else {
    navsetCardBarChartCaption <- 
      htmltools::p(
        htmltools::HTML(
          paste(
            standardText,
            variableKeyText,
            sep = " "
          )
        ), 
        class = "navset-card-caption"
      )
  }
  
  return(navsetCardBarChartCaption)
}

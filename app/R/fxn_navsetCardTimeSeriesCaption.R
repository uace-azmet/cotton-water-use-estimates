#' `fxn_navsetCardTimeSeriesCaption.R` - Build caption for time series graph based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - Data table [[1]] from `fxn_waterUse.R`
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `navsetCardTimeSeriesCaption` Caption for time series graph based on user input


fxn_navsetCardTimeSeriesCaption <- function(azmetStation, inData, startDate, endDate) {
  
  azmetStationStartDate <- 
    dplyr::filter(
      azmetStationMetadata, 
      meta_station_name == azmetStation
    ) %>% 
    dplyr::pull(start_date)
  
  
  if (length(unique(inData$date_year_label)) == 1) {
    standardText <- 
      paste0(
        "Estimated cotton water use (black line in graph) is based on the sum of daily totals during the period of interest. Data for the ", azmetStation, " station in the new AZMet database currently go back to ", gsub(" 0", " ", format(azmetStationStartDate, "%B %d, %Y")), "."
      )
  } else {
    standardText <- 
      paste0(
        "Estimated cotton water use for the current year (black line in graph) is based on the sum of daily totals during the period of interest. Totals for past years (gray lines in graph) are based on the same start and end month and day, but during previous years. Data for the ", azmetStation, " station in the new AZMet database currently go back to ", gsub(" 0", " ", format(azmetStationStartDate, "%B %d, %Y")), "."
      )
  }
  
  variableKeyText <- 
    "Variable key: <strong>Day<sub>season</sub></strong> day number of the growing season defined by the period of interest and since the planting date; <strong>WU<sub>cumulative</sub> (in)</strong> accumulation of daily water use estimates in inches"
  
  # Account for multi-month absence of YUG data in 2021
  nonOperational <- 0
  
  if (azmetStation == "Yuma N.Gila") {
    nodataDateRange <-
      lubridate::interval(
        start = lubridate::date("2021-06-16"),
        end = lubridate::date("2021-10-21")
      )
    
    while (startDate >= azmetStationStartDate) {
      userDateRange <- lubridate::interval(start = startDate, end = endDate)
      
      if (lubridate::int_overlaps(int1 = nodataDateRange, int2 = userDateRange) == TRUE) {
        nonOperational <- 1
      }
      
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    }
  }
  
  # Generate figure footer based on presence/absence of non-operational dates
  if (azmetStation == "Yuma N.Gila" & nonOperational == 1) {
    navsetCardTimeSeriesCaption <- 
      htmltools::p(
        htmltools::HTML(
          paste(
            standardText,
            "However, we do not show cotton water use estimates for the year with a month-day period that overlaps the period from June 16, 2021 through October 10, 2021, when the ", azmetStation, " station was not in operation.",
            variableKeyText,
            sep = " "
          )
        ),
        
        class = "navset-card-caption"
      )
  } else {
    navsetCardTimeSeriesCaption <- 
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
  
  return(navsetCardTimeSeriesCaption)
}

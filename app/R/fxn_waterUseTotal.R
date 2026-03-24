#' `fxn_waterUseTotal` - Calculates total water use for an individual year
#' 
#' @param inData - Derived data table of daily values from `fxn_waterUse.R`
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `waterUseTotal` - Data table with total water use for a single season of an individual year


fxn_waterUseTotal <- function(inData, azmetStation, startDate, endDate) {
  # For x-axis labels and related text of comparison to previous years
  if (lubridate::year(startDate) == lubridate::year(endDate)) { # For data request spanning a single calendar year
    dateYearLabel <- as.character(lubridate::year(startDate))
  } else { # For data request spanning two calendar years
    dateYearLabel <- 
      paste(
        lubridate::year(startDate), 
        lubridate::year(endDate), 
        sep = "-"
      )
  }
  
  if (nrow(inData) == 0) { # For case of empty data return
    waterUseTotal <- data.frame(matrix(
      data = NA,
      nrow = 1, 
      ncol = length(c("meta_station_name", "waterUseTotal", "waterUseTotalLabel", "endDateYear", "dateYearLabel"))
    ))
    
    colnames(waterUseTotal) <- 
      c("meta_station_name", "waterUseTotal", "waterUseTotalLabel", "endDateYear", "dateYearLabel")
    
    waterUseTotal <- waterUseTotal %>%
      dplyr::mutate(meta_station_name = azmetStation) %>%
      dplyr::mutate(waterUseTotal = 0.00) %>%
      dplyr::mutate(waterUseTotalLabel = "NA") %>%
      dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
      dplyr::mutate(dateYearLabel = dateYearLabel)
  } else {
    waterUseTotal <- inData %>%
      # dplyr::group_by(meta_station_name) %>%
      dplyr::summarize(water_use_in_total = sum(water_use_in, na.rm = TRUE)) %>%
      dplyr::rename(waterUseTotal = water_use_in_total) %>%
      dplyr::mutate(waterUseTotalLabel = format(round(waterUseTotal, digits = 2), nsmall = 2)) %>%
      dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
      dplyr::mutate(dateYearLabel = dateYearLabel)
  }
  
  return(waterUseTotal)
}

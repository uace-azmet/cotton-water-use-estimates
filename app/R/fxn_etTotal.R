#' `fxn_etTotal` - Calculates total evapotranspiration for an individual year
#' 
#' @param inData - Derived data table of daily values from `fxn_totalEvapotranspiration.R`
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param etEquation - Evapotranspiration equation selection by user
#' @return `etTotal` - Data table with total evapotranspiration for a single season of an individual year


fxn_etTotal <- function(inData, azmetStation, startDate, endDate, etEquation) {
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
    etTotal <- data.frame(matrix(
      data = NA,
      nrow = 1, 
      ncol = length(c("meta_station_name", "etTotal", "etTotalLabel", "endDateYear", "dateYearLabel"))
    ))
    
    colnames(etTotal) <- 
      c("meta_station_name", "etTotal", "etTotalLabel", "endDateYear", "dateYearLabel")
    
    etTotal <- etTotal %>%
      dplyr::mutate(meta_station_name = azmetStation) %>%
      dplyr::mutate(etTotal = 0.00) %>%
      dplyr::mutate(etTotalLabel = "NA") %>%
      dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
      dplyr::mutate(dateYearLabel = dateYearLabel)
  } else {
    if (etEquation == "Original AZMet") {
      etTotal <- inData %>%
        dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(eto_azmet_in_total = sum(eto_azmet_in, na.rm = TRUE)) %>%
        dplyr::rename(etTotal = eto_azmet_in_total) %>%
        dplyr::mutate(etTotalLabel = format(round(etTotal, digits = 2), nsmall = 2)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (etEquation == "Penman-Monteith") {
      etTotal <- inData %>%
        dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(eto_pen_mon_in_total = sum(eto_pen_mon_in, na.rm = TRUE)) %>%
        dplyr::rename(etTotal = eto_pen_mon_in_total) %>%
        dplyr::mutate(etTotalLabel = format(round(etTotal, digits = 2), nsmall = 2)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    }
  }
  
  return(etTotal)
}

#' `fxn_waterUseSeasonalTotal` - Calculates seasonal total of water use for an individual year
#' 
#' @param inData - Transformed data table of daily values from `fxn_waterUse.R`
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `waterUseSeasonalTotal` - Data table with seasonal total of water use for an individual year


fxn_waterUseSeasonalTotal <- function(inData, azmetStation, startDate, endDate) {
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
    waterUseSeasonalTotal <- data.frame(matrix(
      data = NA,
      nrow = 1, 
      ncol = 
        length(
          c(
            "meta_station_name", 
            "water_use_seasonal_total", 
            "water_use_seasonal_total_label", 
            "end_date_year", 
            "date_year_label"
          )
        )
    ))
    
    colnames(waterUseSeasonalTotal) <- 
      c(
        "meta_station_name", 
        "water_use_seasonal_total", 
        "water_use_seasonal_total_label", 
        "end_date_year", 
        "date_year_label"
      )
    
    waterUseSeasonalTotal <- waterUseSeasonalTotal %>%
      dplyr::mutate(meta_station_name = azmetStation) %>%
      dplyr::mutate(water_use_seasonal_total = NA_real_) %>%
      dplyr::mutate(water_use_seasonal_total_label = "NA") %>%
      dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
      dplyr::mutate(date_year_label = dateYearLabel)
  } else {
    waterUseSeasonalTotal <- inData %>%
      dplyr::summarize(water_use_in_total = sum(water_use_in, na.rm = TRUE)) %>%
      dplyr::rename(water_use_seasonal_total = water_use_in_total) %>%
      dplyr::mutate(water_use_seasonal_total_label = format(round(water_use_seasonal_total, digits = 2), nsmall = 2)) %>%
      dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
      dplyr::mutate(date_year_label = dateYearLabel)
  }
  
  return(waterUseSeasonalTotal)
}

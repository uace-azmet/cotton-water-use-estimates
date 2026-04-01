#' `fxn_waterUseSeasonalTotal` - Calculates seasonal total of water use for an individual year
#' 
#' @param inData - Transformed data table of daily values from `fxn_waterUse.R`
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param userDateRange - date interval based on `startDate` and `endDate`
#' @return `waterUseSeasonalTotal` - Data table with seasonal total of water use for an individual year


fxn_waterUseSeasonalTotal <- function(inData, azmetStation, startDate, endDate, userDateRange) {
  
  waterUseSeasonalTotal <- inData %>%
    dplyr::summarize(water_use_seasonal_total = sum(water_use_in, na.rm = TRUE)) %>%
    # dplyr::rename(water_use_seasonal_total = water_use_in_total) %>%
    dplyr::mutate(
      water_use_seasonal_total_label = 
        format(round(water_use_seasonal_total, digits = 2), nsmall = 2)
    ) %>%
    dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
    dplyr::mutate(
      date_year_label = 
        dplyr::if_else(
          condition = lubridate::year(startDate) == lubridate::year(endDate),
          true = as.character(lubridate::year(startDate)),
          false = paste(lubridate::year(startDate), lubridate::year(endDate), sep = "-")
        )
    )
  
  if (azmetStation == "Yuma N.Gila" & lubridate::int_overlaps(int1 = yugNodataInterval, int2 = userDateRange) == TRUE) {
    waterUseSeasonalTotal$water_use_seasonal_total <- NA_real_
    waterUseSeasonalTotal$water_use_seasonal_total_label <- "NA"
  }
  
  return(waterUseSeasonalTotal)
}

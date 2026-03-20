#' `fxn_azDaily.R` Download AZMet daily data from API-based database
#' 
#' @param azmetStation - AZMet station name
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `azDaily` - Table of downloaded, transformed daily data


fxn_azDaily <- function(azmetStation, startDate, endDate) {
  azDaily <- azmetr::az_daily(
    station_id = 
      dplyr::filter(azmetStationMetadata, meta_station_name == azmetStation) %>% 
      dplyr::pull(meta_station_id),
    start_date = startDate, 
    end_date = endDate
  ) %>% 
    dplyr::select(
      dplyr::all_of(
        c(dailyVarsID, dailyVarsMeasured, dailyVarsDerived) # Defined in `_global.R`
      )
    )
  
  return(azDaily)
}

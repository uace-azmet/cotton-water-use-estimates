#' `fxn_totalEvapotranspiration` - Calculates ET accumulation by day and season for period of interest and individual years
#' 
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param etEquation - Evapotranspiration equation selection by user
#' @return `totalEvapotranspiration` - List of daily [[1]] and seasonal [[2]] data tables of values for individual years


fxn_totalEvapotranspiration <- function(azmetStation, startDate, endDate, etEquation) {
  azmetStationStartDate <- 
    dplyr::filter(azmetStationMetadata, meta_station_name == azmetStation) %>% 
    dplyr::pull(start_date)
    
  azDaily <- 
    fxn_azDaily(
      azmetStation = azmetStation,
      startDate = azmetStationStartDate, # To call API only once, faster with daily data
      endDate = endDate
    )
  
  while (startDate >= azmetStationStartDate) {
    
    # Calculate ET and precipitation accumulation by day for individual year
    singleYearDaily <- 
      dplyr::filter(
        azDaily,
        datetime >= startDate & datetime <= endDate
      ) %>% 
      dplyr::mutate(
        date_year_label = dplyr::if_else(
          condition = lubridate::year(startDate) == lubridate::year(endDate),
          true = as.character(lubridate::year(startDate)),
          false = paste(lubridate::year(startDate), lubridate::year(endDate), sep = "-")
        ),
        day_of_period = dplyr::row_number(),
        eto_azmet_acc = round(cumsum(eto_azmet), digits = 2),
        eto_azmet_in_acc = round(cumsum(eto_azmet_in), digits = 2),
        eto_pen_mon_acc = round(cumsum(eto_pen_mon), digits = 2),
        eto_pen_mon_in_acc = round(cumsum(eto_pen_mon_in), digits = 2),
        precip_total_mm_acc = round(cumsum(precip_total_mm), digits = 2),
        precip_total_in_acc = round(cumsum(precip_total_in), digits = 2)
      )
    
    singleYearTotal <-
      fxn_etTotal(
        inData = singleYearDaily,
        azmetStation = azmetStation,
        startDate = startDate,
        endDate = endDate,
        etEquation = etEquation
      )
    
    # Account for multi-month absence of YUG data in 2021
    if (azmetStation == "Yuma N.Gila") {
      nodataDateRange <-
        lubridate::interval(
          start = lubridate::date("2021-06-16"),
          end = lubridate::date("2021-10-21")
        )

      userDateRange <- lubridate::interval(start = startDate, end = endDate)

      if (lubridate::int_overlaps(int1 = nodataDateRange, int2 = userDateRange) == TRUE) {
        singleYearDaily <- singleYearDaily %>% 
          dplyr::mutate(
            eto_azmet_acc = NA_real_,
            eto_azmet_in_acc = NA_real_,
            eto_pen_mon_acc = NA_real_,
            eto_pen_mon_in_acc = NA_real_,
            precip_total_mm_acc = NA_real_,
            precip_total_in_acc = NA_real_
          )
        
        singleYearTotal$etTotal <- NA_real_
        singleYearTotal$etTotalLabel <- "NA"
      }
    }

    # Build data tables for return
    if (exists("dailyTotals") == FALSE) {
      dailyTotals <- singleYearDaily
    } else {
      dailyTotals <- rbind(dailyTotals, singleYearDaily)
    }
    
    if (exists("seasonalTotals") == FALSE) {
      seasonalTotals <- singleYearTotal
    } else {
      seasonalTotals <- rbind(seasonalTotals, singleYearTotal)
    }
    
    # Setup for analysis of data from previous year
    startDate <- min(seq(lubridate::date(startDate), length = 2, by = "-1 year"))
    endDate <- min(seq(lubridate::date(endDate), length = 2, by = "-1 year"))
  }
  
  return(list(dailyTotals, seasonalTotals))
}

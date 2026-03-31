#' `fxn_waterUse` - Estimates cotton water use by day and season for period of interest and individual years
#' 
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `waterUse` - List of daily [[1]] and seasonal [[2]] data tables of values for individual years


fxn_waterUse <- function(azmetStation, startDate, endDate) {
  
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
    
    userDateRange <- lubridate::interval(start = startDate, end = endDate)
    
    singleYearDaily <- 
      dplyr::filter(azDaily, datetime >= startDate & datetime <= endDate)
    
    if (azmetStation == "Yuma N.Gila" & startDate %within% yugNodataInterval & endDate %within% yugNodataInterval) {
      # Handle empty daily data table at YUG
      singleYearDaily <-
        tibble::tibble( 
          datetime = seq(lubridate::ymd(startDate), lubridate::ymd(endDate), by = "days"),
          meta_station_name = azmetStation,
          eto_pen_mon_in = NA_real_,
          heat_units_55F = NA_real_,
          precip_total_in = NA_real_,
          eto_pen_mon_in_acc = NA_real_,
          heat_units_55F_acc = NA_real_,
          precip_total_in_acc = NA_real_
        )
    } else {
      singleYearDaily <- 
        dplyr::filter(azDaily, datetime >= startDate & datetime <= endDate)
    }
    
    singleYearDaily <- singleYearDaily %>% 
      dplyr::mutate(
        date_year_label = 
          dplyr::if_else(
            condition = lubridate::year(startDate) == lubridate::year(endDate),
            true = as.character(lubridate::year(startDate)),
            false = paste(lubridate::year(startDate), lubridate::year(endDate), sep = "-")
          ),
        day_of_season = dplyr::row_number() - 1,
        eto_pen_mon_in_acc = 
          dplyr::if_else(
            condition = day_of_season == 0,
            true = NA_real_,
            false = dplyr::if_else(
              condition = is.na(eto_pen_mon_in),
              true = NA_real_,
              false = round((cumsum(tidyr::replace_na(eto_pen_mon_in, 0)) - eto_pen_mon_in[1]), digits = 2)
            )
          ),
        heat_units_55F_acc = 
          dplyr::if_else(
            condition = day_of_season == 0,
            true = NA_real_,
            false = dplyr::if_else(
              condition = is.na(heat_units_55F),
              true = NA_real_,
              false = round((cumsum(tidyr::replace_na(heat_units_55F, 0)) - heat_units_55F[1]), digits = 1)
            )
          ),
        precip_total_in_acc = 
          dplyr::if_else(
            condition = day_of_season == 0,
            true = NA_real_,
            false = dplyr::if_else(
              condition = is.na(precip_total_in),
              true = NA_real_,
              false = round((cumsum(tidyr::replace_na(precip_total_in, 0)) - precip_total_in[1]), digits = 2)
            )
          ),
        kc = 
          dplyr::if_else(
            condition = day_of_season == 0,
            true = NA_real_,
            false = dplyr::if_else(
              condition = is.na(heat_units_55F),
              true = NA_real_,
              false = dplyr::if_else(
                condition = heat_units_55F_acc >= 3000,
                true = round(2.3 - (0.0004 * heat_units_55F_acc), digits = 7),
                false = dplyr::if_else(
                  condition = heat_units_55F_acc >= 2000,
                  true = 1.1,
                  false = dplyr::if_else(
                    condition = heat_units_55F_acc >= 600,
                    true = round((0.000743 * heat_units_55F_acc) - 0.33, digits = 7),
                    false = dplyr::if_else(
                      condition = heat_units_55F_acc >= 1,
                      true = 0.1,
                      false = 0
                    )
                  )
                )
              )
            )
          ),
        water_use_in = 
          dplyr::if_else(
            condition = day_of_season == 0,
            true = NA_real_,
            false = dplyr::if_else(
              condition = is.na(eto_pen_mon_in) | is.na(kc),
              true = NA_real_,
              false = dplyr::if_else(
                condition = kc > 0,
                true = round(kc * eto_pen_mon_in, digits = 2),
                false = 0
              ) 
            )
          ),
        water_use_in_acc = 
          dplyr::if_else(
            condition = day_of_season == 0,
            true = NA_real_,
            false = dplyr::if_else(
              condition = is.na(water_use_in),
              true = NA_real_,
              false = round(cumsum(tidyr::replace_na(water_use_in, 0)), digits = 2)
            )
          )
      )
    
    # Account for multi-month absence of YUG data in 2021
    if (azmetStation == "Yuma N.Gila" & lubridate::int_overlaps(int1 = yugNodataInterval, int2 = userDateRange) == TRUE) {
      # Handle partially empty or empty daily data table at YUG
      singleYearDaily <- singleYearDaily %>%
        dplyr::mutate(
          eto_pen_mon_in_acc = 
            dplyr::if_else(
              condition = datetime < yugNodataStartDate,
              true = eto_pen_mon_in_acc,
              false = NA_real_
            ),
          heat_units_55F_acc = 
            dplyr::if_else(
              condition = datetime < yugNodataStartDate,
              true = heat_units_55F_acc,
              false = NA_real_
            ),
          precip_total_in_acc = 
            dplyr::if_else(
              condition = datetime < yugNodataStartDate,
              true = precip_total_in_acc,
              false = NA_real_
            ),
          kc = 
            dplyr::if_else(
              condition = datetime < yugNodataStartDate,
              true = kc,
              false = NA_real_
            ),
          water_use_in = 
            dplyr::if_else(
              condition = datetime < yugNodataStartDate,
              true = water_use_in,
              false = NA_real_
            ),
          water_use_in_acc = 
            dplyr::if_else(
              condition = datetime < yugNodataStartDate,
              true = water_use_in_acc,
              false = NA_real_
            )
        )  
    }
    
    singleYearTotal <-
      fxn_waterUseSeasonalTotal(
        inData = singleYearDaily,
        azmetStation = azmetStation,
        startDate = startDate,
        endDate = endDate,
        userDateRange = userDateRange
      )
    
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

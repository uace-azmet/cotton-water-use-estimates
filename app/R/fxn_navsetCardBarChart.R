#' `fxn_navsetCardBarChart` Generates bar chart of estimated cotton water use of current and recent years
#' 
#' @param inData - Data table [[2]] from `fxn_waterUse.R`
#' @param azmetStation - User-specified AZMet station
#' @return `navsetCardBarChart` - plotly bar chart of estimated cotton water use of current and recent years

# https://plotly-r.com/ 
# https://plotly.com/r/reference/ 
# https://plotly.github.io/schema-viewer/
# https://github.com/plotly/plotly.js/blob/c1ef6911da054f3b16a7abe8fb2d56019988ba14/src/components/fx/hover.js#L1596
# https://www.color-hex.com/color-palette/1041718


fxn_navsetCardBarChart <- function(inData, azmetStation) {
  
  # Inputs -----
  
  averageTotal <- mean(inData$water_use_seasonal_total, na.rm = TRUE)
  
  dataCurrentYear <- inData %>% 
    dplyr::filter(end_date_year == max(end_date_year)) %>%
    dplyr::mutate(end_date_year = as.factor(end_date_year))
  
  dataOtherYears <- inData %>% 
    dplyr::filter(end_date_year != max(end_date_year)) %>% 
    dplyr::mutate(end_date_year = as.factor(end_date_year))
  
  layoutFontFamily <- "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\""
  
  ticktext <- inData$date_year_label
  tickvals <- inData$end_date_year
  
  
  # Bar Chart -----
  
  if (nrow(inData) == 1) { # For stations with only one year of data
    navsetCardBarChart <- 
      plotly::plot_ly( # Bars for `dataOtherYears`
        data = dataOtherYears,
        x = ~end_date_year,
        y = ~water_use_seasonal_total,
        marker = list(color = "#bfbfbf"),
        name = "other years",
        showlegend = FALSE,
        hoverinfo = "text",
        hovertext = 
          ~paste0(
            "<br><b>AZMet Station:</b> ", azmetStation,
            "<br><b>Year:</b> ", date_year_label,
            "<br><b>WU<sub>cumulative</sub>:</b> ", water_use_seasonal_total_label, " inches"
          ),
        type = "bar"
      ) %>% 
      
      plotly::add_trace( # Bar for `dataCurrentYear`
        inherit = FALSE,
        data = dataCurrentYear,
        x = ~end_date_year,
        y = ~water_use_seasonal_total,
        marker = list(color = "#191919"),
        name = "current year",
        showlegend = FALSE,
        hoverinfo = "text",
        hovertext = 
          ~paste0(
            "<br><b>AZMet Station:</b> ", azmetStation,
            "<br><b>Year:</b> ", date_year_label,
            "<br><b>WU<sub>cumulative</sub>:</b> ", water_use_seasonal_total_label, " inches"
          ),
        type = "bar"
      ) %>%
      
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE,
        modeBarButtonsToRemove = 
          c(
            "autoScale2d",
            "hoverClosestCartesian", 
            "hoverCompareCartesian", 
            "lasso2d",
            "select"
          ),
        scrollZoom = FALSE,
        toImageButtonOptions = 
          list(
            format = "png", # Either png, svg, jpeg, or webp
            filename = "AZMet-cotton-water-use-estimates",
            height = 400,
            width = 700,
            scale = 5
          )
      ) %>%
      
      plotly::layout(
        # annotations = list(
        #   align = "left",
        #   font = 
        #     list(
        #       color = "#3b3b3b",
        #       family = layoutFontFamily,
        #       size = 14
        #     ),
        #   showarrow = FALSE,
        #   text = paste("<b>Average: ", format(abs(round(averageTotal, digits = 2)), nsmall = 2), " inches</b>"),
        #   x = 0,
        #   xanchor = "left",
        #   xref = "paper",
        #   xshift = 12,
        #   y = averageTotal,
        #   yanchor = "bottom",
        #   yref = "y",
        #   yshift = 0
        # ),
        font = list(color = "#191919", family = layoutFontFamily, size = 13),
        hoverlabel = list(font = list(family = layoutFontFamily, size = 14)),
        margin = 
          list(
            l = 0,
            r = 0, # For space between plot and modebar
            b = 0,
            t = 0,
            pad = 3 # For space between gridlines and yaxis labels
          ),
        modebar = list(bgcolor = "#FFFFFF", orientation = "v"),
        # shapes =
        #   list(
        #     type = "line",
        #     layer = "above",
        #     line = list(
        #       color = "#3b3b3b", 
        #       dash = "solid",
        #       width = 1
        #     ),
        #     x0 = 0,
        #     x1 = 1,
        #     xref = "paper",
        #     y0 = averageTotal,
        #     y1 = averageTotal,
        #     yref = "y"
        #   ),
        xaxis = 
          list(
            fixedrange = TRUE,
            linewidth = 0,
            ticktext = ticktext,
            tickvals = tickvals,
            title = 
              list(
                font = list(size = 14),
                standoff = 25,
                text = "<b>Year</b>"
              ),
            zeroline = FALSE
          ),
        yaxis = 
          list(
            fixedrange = TRUE,
            gridcolor = "#c9c9c9",
            title = 
              list(
                font = list(size = 14),
                standoff = 25,
                text = "<b>WU<sub>cumulative</sub> (in)</b>"
              ),
            zeroline = TRUE,
            zerolinecolor = "#c9c9c9"
          )
      )
  } else { # For stations with more than one year of data
    navsetCardBarChart <- 
      plotly::plot_ly( # Bars for `dataOtherYears`
        data = dataOtherYears,
        x = ~end_date_year,
        y = ~water_use_seasonal_total,
        marker = list(color = "#bfbfbf"),
        name = "other years",
        showlegend = FALSE,
        hoverinfo = "text",
        hovertext = 
          ~paste0(
            "<br><b>AZMet Station:</b> ", azmetStation,
            "<br><b>Year:</b> ", date_year_label,
            "<br><b>WU<sub>cumulative</sub>:</b> ", water_use_seasonal_total_label, " inches"
          ),
        type = "bar"
      ) %>% 
      
      plotly::add_trace( # Bar for `dataCurrentYear`
        inherit = FALSE,
        data = dataCurrentYear,
        x = ~end_date_year,
        y = ~water_use_seasonal_total,
        marker = list(color = "#191919"),
        name = "current year",
        showlegend = FALSE,
        hoverinfo = "text",
        hovertext = 
          ~paste0(
            "<br><b>AZMet Station:</b> ", azmetStation,
            "<br><b>Year:</b> ", date_year_label,
            "<br><b>WU<sub>cumulative</sub>:</b> ", water_use_seasonal_total_label, " inches"
          ),
        type = "bar"
      ) %>%
      
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE,
        modeBarButtonsToRemove = 
          c(
            "autoScale2d",
            "hoverClosestCartesian", 
            "hoverCompareCartesian", 
            "lasso2d",
            "select"
          ),
        scrollZoom = FALSE,
        toImageButtonOptions = 
          list(
            format = "png", # Either png, svg, jpeg, or webp
            filename = "AZMet-cotton-water-use-estimates",
            height = 400,
            width = 700,
            scale = 5
          )
      ) %>%
      
      plotly::layout(
        annotations = 
          list(
            align = "left",
            font = list(color = "#808080", family = layoutFontFamily, size = 14),
            showarrow = FALSE,
            text = 
              paste(
                "<b>Average: ", format(abs(round(averageTotal, digits = 2)), nsmall = 2), " inches</b>"
              ),
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 12,
            y = averageTotal,
            yanchor = "bottom",
            yref = "y",
            yshift = 0
          ),
        font = list(color = "#191919", family = layoutFontFamily, size = 13),
        hoverlabel = list(font = list(family = layoutFontFamily, size = 14)),
        margin = 
          list(
            l = 0,
            r = 0, # For space between plot and modebar
            b = 0,
            t = 0,
            pad = 3 # For space between gridlines and yaxis labels
          ),
        modebar = list(bgcolor = "#FFFFFF", orientation = "v"),
        shapes =
          list(
            type = "line",
            layer = "above",
            line = list(color = "#808080", dash = "solid", width = 1),
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = averageTotal,
            y1 = averageTotal,
            yref = "y"
          ),
        xaxis = 
          list(
            fixedrange = TRUE,
            linewidth = 0,
            ticktext = ticktext,
            tickvals = tickvals,
            title = 
              list(
                font = list(size = 14),
                standoff = 25,
                text = "<b>Year</b>"
              ),
            zeroline = FALSE
          ),
        yaxis = 
          list(
            fixedrange = TRUE,
            gridcolor = "#c9c9c9",
            title = 
              list(
                font = list(size = 14),
                standoff = 25,
                text = "<b>WU<sub>cumulative</sub> (in)</b>"
              ),
            zeroline = TRUE,
            zerolinecolor = "#c9c9c9"
          )
      )
  }
  
  return(navsetCardBarChart)
}

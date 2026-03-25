#' `fxn_navsetCardTable.R` - Build summary table with daily data
#' 
#' @param inData - Data table [[1]] from `fxn_waterUse.R`
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `navsetCardTable` - Summary table of daily data, reactable format


fxn_navsetCardTable <- function(inData, startDate, endDate) {
  
  # Inputs -----
  
  inData <- inData %>% 
    dplyr::filter(datetime >= startDate & datetime <= endDate)
  
  inData <- inData %>%
    dplyr::select(
      dplyr::all_of(
        c(
          "meta_station_name",
          "datetime",
          "day_of_season",
          "water_use_in",
          "water_use_in_acc",
          "kc",
          "eto_pen_mon_in", 
          "eto_pen_mon_in_acc",
          "heat_units_55F",
          "heat_units_55F_acc",
          "precip_total_in", 
          "precip_total_in_acc"
        )
      )
    ) %>% 
    dplyr::rename(
      et_total_in = eto_pen_mon_in,
      et_total_in_acc = eto_pen_mon_in_acc
    )
  
  
  # Table -----
  
  navsetCardTable <- inData |>
    reactable::reactable(
      columns = list(
        meta_station_name = reactable::colDef(
          name = "Station",
          #aggregate = NULL,
          #sortable = NULL,
          #resizable = NULL,
          #filterable = NULL,
          #searchable = NULL,
          #filterMethod = NULL,
          #show = TRUE,
          #defaultSortOrder = NULL,
          #sortNALast = FALSE,
          #format = NULL,
          #cell = NULL,
          #grouped = NULL,
          #aggregated = NULL,
          #header = NULL,
          #footer = NULL,
          #details = NULL,
          #filterInput = NULL,
          html = TRUE,
          na = "NA",
          rowHeader = FALSE,
          minWidth = 150,
          #maxWidth = NULL,
          #width = NULL,
          #align = NULL,
          #vAlign = NULL,
          #headerVAlign = NULL,
          # sticky = "left",
          # class = "table-reactable-column-station",
          # style = list(
          #   borderRight = "1px solid #989898",
          #   boxShadow = "1px 0px 0px 0px #e3e3e3"
          # ),
          #headerClass = NULL,
          # headerStyle = list(
          #   borderRight = "1px solid #989898",
          #   boxShadow = "1px 1px 0px 0px #e3e3e3"
          # ),
          #footerClass = NULL,
          #footerStyle = NULL
        ), 
        datetime = reactable::colDef(
          name = "Date",
          html = TRUE,
          # minWidth = 100,
          na = "NA",
          rowHeader = TRUE
        ),
        day_of_season = reactable::colDef(
          name = htmltools::HTML("Day<sub>season</sub><br>"),
          html = TRUE,
          # minWidth = 100,
          na = "NA",
          rowHeader = TRUE
        ),
        water_use_in = reactable::colDef(
          name = 
            htmltools::HTML(
              paste0(
                "WU<br>", 
                tags$span(style = "font-weight: normal; font-size: 0.8rem", "(in)")
              )
            ),
          format = reactable::colFormat(digits = 2),
          html = TRUE,
          na = "NA",
          rowHeader = TRUE
        ),
        water_use_in_acc = reactable::colDef(
          name = 
            htmltools::HTML(
              paste0(
                "WU<sub>cumulative</sub><br>",
                tags$span(style = "font-weight: normal; font-size: 0.8rem", "(in)")
              )
            ),
          format = reactable::colFormat(digits = 2),
          html = TRUE,
          na = "NA",
          rowHeader = TRUE
        ),
        et_total_in = reactable::colDef(
          name = 
            htmltools::HTML(
              paste0(
                "ET<br>", 
                tags$span(style = "font-weight: normal; font-size: 0.8rem", "(in)")
              )
            ),
          format = reactable::colFormat(digits = 2),
          html = TRUE,
          na = "NA",
          rowHeader = TRUE
        ),
        et_total_in_acc = reactable::colDef(
          name = 
            htmltools::HTML(
              paste0(
                "ET<sub>cumulative</sub><br>",
                tags$span(style = "font-weight: normal; font-size: 0.8rem", "(in)")
              )
            ),
          format = reactable::colFormat(digits = 2),
          html = TRUE,
          na = "NA",
          rowHeader = TRUE
        ),
        kc = reactable::colDef(
          name = htmltools::HTML("K<sub>c</sub><br>"),
          format = reactable::colFormat(digits = 3),
          html = TRUE,
          # minWidth = 100,
          na = "NA",
          rowHeader = TRUE
        ),
        heat_units_55F = reactable::colDef(
          name = 
            htmltools::HTML(
              paste0(
                "HU<br>", 
                tags$span(style = "font-weight: normal; font-size: 0.8rem", "(DD °F)")
              )
            ),
          format = reactable::colFormat(digits = 1),
          html = TRUE,
          na = "NA",
          rowHeader = TRUE
        ),
        heat_units_55F_acc = reactable::colDef(
          name = 
            htmltools::HTML(
              paste0(
                "HU<sub>cumulative</sub><br>",
                tags$span(style = "font-weight: normal; font-size: 0.8rem", "(DD °F)")
              )
            ),
          format = reactable::colFormat(digits = 1),
          html = TRUE,
          na = "NA",
          rowHeader = TRUE
        ),
        precip_total_in = reactable::colDef(
          name = 
            htmltools::HTML(
              paste0(
                "P<br>", 
                tags$span(style = "font-weight: normal; font-size: 0.8rem", "(in)")
              )
            ),
          format = reactable::colFormat(digits = 2),
          html = TRUE,
          na = "NA",
          rowHeader = TRUE
        ),
        precip_total_in_acc = reactable::colDef(
          name = 
            htmltools::HTML(
              paste0(
                "P<sub>cumulative</sub><br>",
                tags$span(style = "font-weight: normal; font-size: 0.8rem", "(in)")
              )
            ),
          format = reactable::colFormat(digits = 2),
          html = TRUE,
          na = "NA",
          rowHeader = TRUE
        )
      ),
      #columnGroups = NULL,
      rownames = FALSE,
      #groupBy = NULL,
      sortable = FALSE,
      resizable = FALSE,
      filterable = FALSE,
      searchable = FALSE,
      searchMethod = NULL,
      #defaultColDef = NULL,
      #defaultColGroup = NULL,
      #defaultSortOrder = "asc",
      #defaultSorted = NULL,
      pagination = FALSE,
      #defaultPageSize = 10,
      showPageSizeOptions = FALSE,
      #pageSizeOptions = c(10, 25, 50, 100),
      #paginationType = "numbers",
      showPagination = NULL,
      showPageInfo = FALSE,
      #minRows = 1,
      #paginateSubRows = FALSE,
      #details = NULL,
      defaultExpanded = TRUE,
      selection = NULL,
      defaultSelected = NULL,
      onClick = NULL,
      highlight = TRUE,
      outlined = FALSE,
      bordered = FALSE,
      borderless = TRUE,
      striped = TRUE,
      compact = TRUE,
      #wrap = TRUE,
      #showSortIcon = TRUE,
      #showSortable = FALSE,
      class = "navset-card-table",
      #style = NULL,
      #rowClass = NULL,
      #rowStyle = NULL,
      fullWidth = TRUE,
      width = "auto",
      # height = 400,
      theme = 
        reactable::reactableTheme(
          color = NULL,
          backgroundColor = "#FFFFFF",
          borderColor = "#dee2e6",
          borderWidth = "2px",
          stripedColor = NULL,
          highlightColor = NULL,
          cellPadding = NULL,
          style = NULL,
          tableStyle = NULL,
          headerStyle = 
            list(
              color = "#191919", 
              fontFamily = "monospace", 
              fontSize = "0.8rem",
              borderBottomColor = rgb(180/255, 180/255, 180/255, 1.0),
              borderBottomWidth = "1px",
              boxShadow = "0px 1px 0px 0px #e3e3e3"
            ),
          groupHeaderStyle = NULL,
          tableBodyStyle = NULL,
          rowGroupStyle = NULL,
          rowStyle = NULL,
          rowStripedStyle = NULL,
          rowHighlightStyle = NULL,
          rowSelectedStyle = NULL,
          cellStyle = list(color = "#191919", fontFamily = "monospace", fontSize = "0.8rem"),
          footerStyle = NULL,
          inputStyle = NULL,
          filterInputStyle = NULL,
          searchInputStyle = NULL,
          selectStyle = NULL,
          paginationStyle = NULL,
          pageButtonStyle = NULL,
          pageButtonHoverStyle = NULL,
          pageButtonActiveStyle = NULL,
          pageButtonCurrentStyle = NULL
        ),
      #language = getOption("reactable.language"),
      #meta = NULL,
      #elementId = NULL,
      #static = getOption("reactable.static", FALSE),
      #selectionId = NULL
    )
  
  return(navsetCardTable)
}

#' `fxn_figureTitle.R` - Build title for figure
#' 
#' @param azmetStation - AZMet station selected by user
#' @return `figureTitle` - Title for figure based on selected station


fxn_figureTitle <- function(azmetStation, titleIcon) {
  figureTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          # bsicons::bs_icon("graph-up", class = "bolder-icon"),
          bsicons::bs_icon(titleIcon),
          # bsicons::bs_icon("graph-up"), moisture file-earmark-post "bar-chart-fill"
          htmltools::HTML("&nbsp;"),
          htmltools::HTML("&nbsp;"),
          toupper(
            htmltools::HTML(paste0(
              "<strong>Estimated Cotton Water Use at the AZMet ", azmetStation, " Station</strong>"
            ))
          ),
          htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Select from the tabs below to view different presentations of the data.",
            id = "infoFigureTitle",
            placement = "right"
          )
        ),
      ),
      
      class = "figure-title"
    )
  
  return(figureTitle)
}

#' `fxn_navsetCardTabTitle.R` - Build title for navset card tab section
#' 
#' @param azmetStation - AZMet station selected by user
#' @return `navsetCardTabTitle` - Title for navset card tab section based on user input


fxn_navsetCardTabTitle <- function(azmetStation, titleIcon) {
  navsetCardTabTitle <- 
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
            id = "infoNavsetCardTabTitle",
            placement = "right"
          )
        ),
      ),
      
      class = "navset-card-tab-title"
    )
  
  return(navsetCardTabTitle)
}

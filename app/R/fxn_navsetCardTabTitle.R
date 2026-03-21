#' `fxn_navsetCardTabTitle.R` - Build title for navset card tab section of page
#' 
#' @param azmetStation - AZMet station selection by user
#' @param navsetCardTabTitleIcon - Title icon based on selected tab
#' @return `navsetCardTabTitle` - Title for navset card tab section of page based on user input


fxn_navsetCardTabTitle <- function(azmetStation, navsetCardTabTitleIcon) {
  if (navsetCardTabTitleIcon == "bar-chart-fill") {
    iconClass = "normal-icon"
  } else {
    iconClass = "bolder-icon"
  }
  
  navsetCardTabTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon(navsetCardTabTitleIcon, class = iconClass),
          htmltools::HTML("&nbsp;&nbsp;"),
          toupper(
            htmltools::HTML(
              paste0(
                "<strong>Estimated cotton water use at the AZMet ", azmetStation, " Station</strong>"
              ),
            )
          ),
          htmltools::HTML("&nbsp;"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "In addition to the following summary, select from the tabs below to view the estimated water use in different contexts.",
            id = "infoNavsetCardTabTitle",
            placement = "right"
          )
        )
      ),
      
      class = "navset-card-tab-title"
    )
  
  return(navsetCardTabTitle)
}

#' `fxn_navsetCardTabTitle.R` - Build title for navset card tab section
#' 
#' @param azmetStation - AZMet station selected by user
#' @param navsetCardTabTitleIcon - Title icon based on selected tab
#' @return `navsetCardTabTitle` - Title for navset card tab section based on user input


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
                "<strong>Estimated Cotton Water Use at the AZMet ", azmetStation, " Station</strong>"
              )
            )
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

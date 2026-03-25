#' `fxn_navsetCardTableCaption.R` - Build caption for table summary based on user input
#' 
#' @return `navsetCardTableCaption` Caption for table summary based on user input


fxn_navsetCardTableCaption <- function() {
  navsetCardTableCaption <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Variable key: <strong>Day<sub>season</sub></strong> day number of the growing season defined by the period of interest and since the planting date; <strong>ET</strong> daily total evapotranspiration in inches as estimated by the ", "etEquation", " equation; <strong>ET<sub>cumulative</sub></strong> accumulation of daily total evapotranspiration in inches during the period of interest as estimated by the ", "etEquation", " equation; <strong>P</strong> daily total precipitation in inches; <strong>P<sub>cumulative</sub></strong> accumulation of daily total precipitation in inches during the period of interest"
        )
      ),
      
      class = "navset-card-caption"
    )
  
  return(navsetCardTableCaption)
}

#' `fxn_navsetCardTableCaption.R` - Build caption for table summary based on user input
#' 
#' @return `navsetCardTableCaption` Caption for table summary based on user input


fxn_navsetCardTableCaption <- function() {
  navsetCardTableCaption <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Values of 'NA' when Day<sub>season</sub> equals 0 reflect no water use or accumulation on the planting date. Otherwise, values of 'NA' denote no data. Variable key: <strong>Day<sub>season</sub></strong> day number of the growing season defined by the period of interest and since the planting date; <strong>ET</strong> daily total evapotranspiration in inches; <strong>ET<sub>cumulative</sub></strong> accumulation of daily total evapotranspiration in inches during the period of interest; <strong>HU</strong> daily total of heat units<sub>86-55 °F</sub> in degree-days Fahrenheit; <strong>HU<sub>cumulative</sub></strong> accumulation of daily total of heat units<sub>86-55 °F</sub> in degree-days Fahrenheit during the period of interest; <strong>K<sub>c</sub></strong> crop coefficient; <strong>P</strong> daily total precipitation in inches; <strong>P<sub>cumulative</sub></strong> accumulation of daily total precipitation in inches during the period of interest; <strong>WU</strong> daily water use estimates in inches; <strong>WU<sub>cumulative</sub></strong> accumulation of daily water use estimates in inches during the period of interest"
        )
      ),
      
      class = "navset-card-caption"
    )
  
  return(navsetCardTableCaption)
}

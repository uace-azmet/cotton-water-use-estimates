datepickerYumaNGilaErrorModal <- 
  shiny::modalDialog(
    htmltools::HTML(
      "<em>The Yuma N.Gila station was not in operation from June 16, 2021 through October 21, 2021. To specify a period of interest during that year, please select both a <b>Planting Date</b> and <b>End Date</b> prior to or after this period."
    ),
    easyClose = FALSE,
    fade = FALSE,
    footer = shiny::modalButton("CLOSE"),
    size = "s",
    title = 
      htmltools::p(
        bsicons::bs_icon("sliders", class = "bolder-icon"), 
        htmltools::HTML("&nbsp;<b>DATA OPTIONS</b>")
      )
  )

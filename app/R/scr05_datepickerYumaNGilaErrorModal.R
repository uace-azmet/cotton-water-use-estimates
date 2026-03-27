datepickerYumaNGilaErrorModal <- 
  shiny::modalDialog(
    shiny::em(
      "The Yuma N.Gila station was not in operation from June 16, 2021 through October 10, 2021. Please select a 'Planting Date' prior to this period."
    ),
    easyClose = FALSE,
    fade = FALSE,
    footer = shiny::modalButton("CLOSE"),
    size = "s",
    title = htmltools::p(
      id = "datepickerModal",
      bsicons::bs_icon("sliders", class = "bolder-icon"), 
      htmltools::HTML("&nbsp;<b>DATA OPTIONS</b>")
    )
  )

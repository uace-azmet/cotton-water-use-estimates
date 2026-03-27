datepickerErrorModal <- 
  shiny::modalDialog(
    shiny::em(
      "Please select a 'Planting Date' that is earlier than or the same as the 'End Date'."
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

#' `fxn_downloadButtonsDiv.R` - Build HTML div with download .csv and .tsv buttons
#' 
#' @return `downloadButtonsDiv` - HTML div with download .csv and .tsv buttons


fxn_downloadButtonsDiv <- function() {
  downloadButtonsDiv <- 
    htmltools::div(
      shiny::downloadButton(
        "downloadCSV", 
        label = "Download .csv", 
        class = "btn btn-default btn-blue", 
        type = "button"
      ),
      shiny::downloadButton(
        "downloadTSV", 
        label = "Download .tsv", 
        class = "btn btn-default btn-blue", 
        type = "button"
      ),
      htmltools::HTML("&nbsp;&nbsp;"),
      bslib::tooltip(
        bsicons::bs_icon("info-circle"),
        "Click or tap to download a file of the input data for the above information with either comma- or tab-separated values.",
        id = "downloadInfo",
        placement = "right"
      ),
      
      class = "download-buttons-div",
      style = "display: flex; align-items: top; gap: 0px;", # Flexbox styling
    )
  
  return(downloadButtonsDiv)
}

# init.R
#
# Example R code to install packages if not already installed
#
my_packages = c("httr", "shinyjs", "DT", "ggplot2", "tidyr", "dplyr", "googledrive", "shiny", "googlesheets4", 
                "shinydashboard", "RCurl", "googledrive", "jpeg", "RSQLite", "shinyWidgets", "xml2", "shinyStore",
                "colourpicker", "daterangepicker", "timevis", "lubridate", "jsonlite", "RPostgres", "DBI")
install_if_missing = function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}
invisible(sapply(my_packages, install_if_missing))

if (!require("shinyStore")) {
  devtools::install_github("trestletech/shinyStore")
}

if (!require("devtools")) {
  install.packages("devtools")
}

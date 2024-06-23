# init.R
#
# Example R code to install packages if not already installed
#
my_packages <- c("httr", "shinyjs", "DT", "ggplot2", "tidyr", "dplyr", "googledrive", "shiny", "googlesheets4", 
                 "shinydashboard", "RCurl", "googledrive", "jpeg", "RSQLite", "shinyWidgets", "xml2", "colourpicker", 
                 "daterangepicker", "timevis", "lubridate", "jsonlite", "RPostgres", "DBI", "blastula")

install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}

invisible(sapply(my_packages, install_if_missing))

# Ensure 'remotes' package is installed for installing from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install shinyStore from GitHub if not already installed
if (!requireNamespace("shinyStore", quietly = TRUE)) {
  remotes::install_github("trestletech/shinyStore")
}

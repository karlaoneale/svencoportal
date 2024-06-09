# init.R
#
# Example R code to install packages if not already installed
#
install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }

install_if_missing("githubinstall")
library(githubinstall)
githubinstall("shinyStore")
my_packages = c("httr", "shinyjs", "DT", "ggplot2", "tidyr", "dplyr", "googledrive", "shiny", "googlesheets4", 
                "shinydashboard", "RCurl", "googledrive", "jpeg", "RSQLite", "shinyWidgets", "xml2",
                "colourpicker", "daterangepicker", "timevis", "lubridate", "jsonlite", "RPostgres", "DBI")
}
invisible(sapply(my_packages, install_if_missing))

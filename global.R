library(shinydashboard)
library(googlesheets4)
library(shiny)
library(googledrive)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(shinyjs)
library(httr)
library(jsonlite)
library(lubridate)
library(googlesheets4)
library(timevis)
library(daterangepicker)
library(colourpicker)
library(xml2)
library(shinyWidgets)
library(RSQLite)
library(jpeg)  # or use 'png' if your image is in PNG format
library(googledrive)
library(RCurl)

source("functions.R")
google_drive_service_acc <- "svencoportal-46a05025cc4a.json"
print(Sys.getenv("SAGE_API_URL"))
Sys.getenv("SAGE_API_URL")
Sys.getenv("SAGE_API_URL")
api_url <- Sys.getenv("SAGE_API_URL")
username <- Sys.getenv("SAGE_USER")
password <- Sys.getenv("SAGE_PASS")
companyid <- Sys.getenv("SAGE_COMPANY_ID")
breakfast_task <- 94538
default_task <- 94121
apikey <- Sys.getenv("SAGE_API_KEY")

wa_api_url <- Sys.getenv("WA_API_URL")
webhook_url <- Sys.getenv("WEBHOOK_URL")
wa_from <- "206349329226876"
wa_token <- Sys.getenv("WA_TOKEN")

task_ids <- c("Fitting & Assembly" = "100989","Machining" = "93868","Other" = "94121","Welding & Fabrication" = "93864", "Breakfast Break" = "94538", "Cleaning" = "94538", "Driving Duty" = "93869", "HOD Meeting" = "145048", "Workshop Maintenance" = "116519")
task_colors <- c("Fitting & Assembly" = "#118ab2","Machining" = "#06d6a0","Other" = "#ffd166","Welding & Fabrication" = "#ef476f", "Breakfast Break" = "#ff6b6b", "Cleaning" = "#7209b7", "Driving Duty" = "#ff9f1c", "HOD Meeting" = "#6a0572", "Workshop Maintenance" = "#118ab2")
progress_icons <- c("Not Started" = "circle", "In Progress" = "circle-half-stroke", "Completed" = "circle")
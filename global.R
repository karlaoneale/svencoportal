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

api_url <- "https://accounting.sageone.co.za/api/2.0.0/"
username <- "karlaoneale@gmail.com"
password <- "jcnqixra70!U"
companyid <- "468904"
breakfast_task <- 94538
default_task <- 94121
apikey <- "{9AA50388-5458-4B4D-9875-8E77F94A05F1}"

wa_api_url <- "https://graph.facebook.com/v19.0/"
webhook_url <- "https://sedate-iced-nemophila.glitch.me/allMessages"
wa_from <- "256491817549838"
wa_token <- "EAAFaZBU3CdR4BOzWwoDOcpOzPnpk2rZA9TtAUkIomZCkQSUK9pAn4hhlHqZBpzfmKJR9SlsQHZAlNZC4MqctBZBgtZBP87aoo00SQlxZB3rZCG9Ol5T9U7ZBCxKtg714ZBQ7Yij8PEPH42BDqnumrhEdECwEFwctZAuiibrR4qvwoQaGAphqeWjbyMKZAEpEVz471tipOe"

task_ids <- c("Fitting & Assembly" = "100989","Machining" = "93868","Other" = "94121","Welding & Fabrication" = "93864", "Breakfast Break" = "94538", "Cleaning" = "94538", "Driving Duty" = "93869", "HOD Meeting" = "145048", "Workshop Maintenance" = "116519")
task_colors <- c("Fitting & Assembly" = "#118ab2","Machining" = "#06d6a0","Other" = "#ffd166","Welding & Fabrication" = "#ef476f", "Breakfast Break" = "#ff6b6b", "Cleaning" = "#7209b7", "Driving Duty" = "#ff9f1c", "HOD Meeting" = "#6a0572", "Workshop Maintenance" = "#118ab2")
progress_icons <- c("Not Started" = "circle", "In Progress" = "circle-half-stroke", "Completed" = "circle")

templates_that_req_actions <- c("task_complete", "task_time", "task_time_reminder", "request_payemt_without_quote", "invoice_approval", "quote_approval",
                                "request_quote_pmt", "all_tasks_completed", "start_invoice", "new_order", "quote_no_pmt")

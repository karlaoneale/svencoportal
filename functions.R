con <- dbConnect(RPostgres::Postgres(), user = "u2tnmv2ufe7rpk", password = "p899046d336be15351280fd542015420a8e18e22dfe07c1cccaaa8e0e9fb20631", host = "cdgn4ufq38ipd0.cluster-czz5s0kz4scl.eu-west-1.rds.amazonaws.com", port = 5432, dbname = "d6qh1puq26hrth")

get_query <- function(query) {
  a <- tryCatch({
    a <- dbGetQuery(con, query)
  }, 
  error = function(e) {
    dbDisconnect(con)
    con <- dbConnect(RPostgres::Postgres(), user = "u2tnmv2ufe7rpk", password = "p899046d336be15351280fd542015420a8e18e22dfe07c1cccaaa8e0e9fb20631", host = "cdgn4ufq38ipd0.cluster-czz5s0kz4scl.eu-west-1.rds.amazonaws.com", port = 5432, dbname = "d6qh1puq26hrth")
    a <- dbGetQuery(con, query)  
  })
  return(a)
}

execute <- function(query) {
  tryCatch({
    dbExecute(con, query)
  }, 
  error = function(e) {
    dbDisconnect(con)
    con <- dbConnect(RPostgres::Postgres(), user = "u2tnmv2ufe7rpk", password = "p899046d336be15351280fd542015420a8e18e22dfe07c1cccaaa8e0e9fb20631", host = "cdgn4ufq38ipd0.cluster-czz5s0kz4scl.eu-west-1.rds.amazonaws.com", port = 5432, dbname = "d6qh1puq26hrth")
    dbExecute(con, query)        
  })
}

get_pm_wa <- function() {return(get_query("SELECT wa_number FROM active_ts WHERE pm = 'true';")$wa_number)}

get_ac_wa <- function() {return(get_query("SELECT wa_number FROM active_ts WHERE ac = 'true';")$wa_number)}

get_ai_wa <- function() {return(get_query("SELECT wa_number FROM active_ts WHERE ai = 'true';")$wa_number)}

get_ap_wa <- function() {return(get_query("SELECT wa_number FROM active_ts WHERE ap = 'true';")$wa_number)}

get_md_wa <- function() {return(get_query("SELECT wa_number FROM active_ts WHERE md = 'true';")$wa_number)}

get_wa_id <- function(name) {return(get_query(paste0("SELECT wa_number FROM active_ts WHERE name = '",name,"';"))$wa_number)}

get_from_api <- function(param, action = "Get", query_params = "") {
  url <- paste0(api_url, param, "/", action, "?", URLencode(query_params), "companyid=", companyid, "&apikey=", apikey)
  response <- GET(url, authenticate(username, password))
  
  # Check the response status
  status_code <- response$status_code
  if (status_code == 200) {
    api_data <- content(response, "text", encoding = "UTF-8")
    parsed_data <- fromJSON(api_data)
  } else {
    resetPage()
    stop(paste("Error: API request failed with status code", status_code))
  }
}

get_tasks <- function(parsed_data) {
  ProjectTasks <- parsed_data$Results$ProjectTasks
  proj_vector <- c()
  for (task in ProjectTasks) {
    proj_vector <- c(proj_vector, paste(task$ProjectID, collapse = ","))
  }
  extracted_data <- data.frame(
    ID = parsed_data$Results$ID,
    Name = parsed_data$Results$Name,
    ProjectID = proj_vector
  )
  
}

get_users <- function(parsed_data) {
  extracted_data <- data.frame("FirstName"=parsed_data$Results$FirstName, "ID"=parsed_data$Results$ID)
}

post_to_api <- function(param, data) {
  url <- paste0(api_url,param,"/Save?companyid=",companyid,"&apikey=",apikey)
  body <- toJSON(as.list(data), auto_unbox = TRUE)
  response <- POST(url, encode = "json", body=body, authenticate(username, password),add_headers("Content-Type" = "application/json"))
  
  # Check the response status
  status_code <- response$status_code
  if (status_code == 201) {
    api_data <- content(response, "text", encoding = "UTF-8")
    parsed_data <- fromJSON(api_data)
  } else {
    resetPage()
    stop(paste("Error: API request failed with status code", status_code))
  }
}

post_to_api1 <- function(param, data) {
  url <- paste0(api_url,param,"/Save?companyid=",companyid,"&apikey=",apikey)
  body <- toJSON(as.list(data), auto_unbox = TRUE)
  response <- POST(url, encode = "json", body=body, authenticate(username, password),add_headers("Content-Type" = "application/json"))
  
  # Check the response status
  status_code <- response$status_code
  api_data <- content(response, "text", encoding = "UTF-8")
  if (status_code == 201) {
    parsed_data <- fromJSON(api_data)
    return(parsed_data)
  } else {
    return(api_data)
  }
}

resetPage <- function() {
  proxy <- dataTableProxy("dt_employees")
  proxy %>% selectRows(selected = NULL)
  shinyjs::show("employees")
  shinyjs::hide("projects")
}

templateTask <- function(task, description, project, progress) {
  sprintf(
    '
    <table><tbody>
      <tr>
        <td><strong style="font-size: x-small;">%s:</strong> %s</td>
      </tr>
      <tr>
        <td style="font-size: x-small;">%s: <strong><em>%s</em></strong></td>
      </tr>
    </tbody></table>',
    project, progress, task, description
  )
}

send_template <- function(wa_id, body_params = NULL, template_name, heading = NULL, taskid=NA, project=NA, orderid=NA, docid=NA, invoicename=NA) {
  url <- paste0(wa_api_url,wa_from,"/messages")
  b <- ""
  h <- ""
  if (!is.null(heading)) {
    if (heading$type == "text") {
      heading <- lapply(heading, function(x) {
        ifelse(is.null(x$text) || x$text == "" || is.na(x$text), " ", x$text)
      })
    }
    h <- list("type" = "header","parameters" = list(heading))
  }
  if (!is.null(body_params)) {
    for (i in 1:length(body_params)) {
      body_params[[i]]$text <- ifelse(is.null(body_params[[i]]$text) || body_params[[i]]$text == "" || is.na(body_params[[i]]$text), " ", body_params[[i]]$text)
    }
    b <- list("type" = "body","parameters" = body_params)
  }
  body <- list(
    "messaging_product" = "whatsapp",
    "to" = wa_id,
    "type" = "template",
    "template" = list(
      "name" = template_name,
      "language" = list("code" = "en_US"),
      "components" = list(b, h)
    )
  )
  
  response <- tryCatch({
    response <- POST(url, encode = "json", body=body, add_headers("Authorization" = paste0("Bearer ",wa_token), "Content-Type" = "application/json"))
  }, 
  error = function(e) {
    response <- POST(url, encode = "json", body=body, add_headers("Authorization" = paste0("Bearer ",wa_token), "Content-Type" = "application/json"))
  })
  
  # Check the response status
  status_code <- response$status_code
  if (status_code %in% c(200, 201)) {
    api_data <- content(response, "text", encoding = "UTF-8")
    parsed_data <- fromJSON(api_data)
    message_id <- fromJSON(rawToChar(response$content))[["messages"]][["id"]]
    if (template_name %in% templates_that_req_actions) {
      execute(paste0("INSERT INTO sent_wa (id, timestamp, recipientid, task, project, orderid, docid, invoicename, message, responded) VALUES ('",message_id 
                     , "',", as.numeric(Sys.time()),
                     ",'", wa_id, "','", taskid, "','", project, "','", orderid, "','", docid,"', '",invoicename,"','",template_name,"', 'false');"))
    } else {
      execute(paste0("INSERT INTO sent_wa (id, timestamp, recipientid, task, project, orderid, docid, invoicename, message, responded) VALUES ('",message_id 
                     , "',", as.numeric(Sys.time()),
                     ",'", wa_id, "','", taskid, "','", project, "','", orderid, "','", docid,"', '",invoicename,"','",template_name,"', 'true');"))
    }
    
    print(paste0("Sent ",template_name," template to ",wa_id))
    return(TRUE)
  } else {
    print(paste("Error: API request failed with status code", status_code, " TEMPLATE: ", template_name, "\n ", body_params))
    return(FALSE)
  }
}

get_wa_id <- function(name) {
  return("27814104435")
}

updateProjTimevis <- function(df) {
  d <-data.frame(
    id = df$taskid,
    content = templateTask(df$taskname, df$description, df$projectname, df$status),
    start = df$plannedstart,
    end = df$plannedcompletion,
    style = paste("background-color:", df$colour, "; font-size:5px"),
    type = "range"
  ) %>% 
    mutate_all(as.factor)
}

uploadToGoogleDrive <- function(image_binary, project_name, folder_name, filename = NULL) {
  drive_auth(path = google_drive_service_acc)
  temp_image <- tempfile(fileext = ".jpg")
  writeBin(image_binary, temp_image)
  path <- paste0(folder_name,"/",project_name,"/")
  if (is.null(filename)) {
    filename <- paste0(
      format(Sys.Date(), format = "%d_%m_%Y"),
      "_",
      paste0(sample(c(0:9, letters, LETTERS), 5, replace = TRUE), collapse = ""),
      ".jpg"
    )
  }
  
  try(path <- drive_mkdir(project_name, path = paste0(folder_name,"/"), overwrite = FALSE))
  # Create a new file in the specified folder
  drive_upload(temp_image, 
               name = filename, 
               type = "image/jpeg", 
               path = path)
  
  # Clean up temporary file
  unlink(temp_image)
  
  folder_contents <- drive_ls(folder_name)
  folder <- folder_contents[folder_contents$name == project_name, ]
  return(folder$drive_resource[[1]]$webViewLink)
}

create_order <- function(message_details, drive_link = NULL) {
  lastid <- max(get_query(paste0("SELECT id FROM orders;"))$id)
  projNumber <- substr(message_details$text, 2, 5)
  itemDescription <- trimws(substr(message_details$text, 7, nchar(message_details$text)), "left") 
  from <- (get_query(paste0("SELECT name FROM active_ts WHERE wa_number = '",message_details$from,"';"))$name)[1]
  
  projectName <- get_query(paste0("SELECT projectname FROM projects WHERE projectname LIKE '", projNumber, "%';"))$projectname
  
  if (is.null(drive_link)) {
    execute(paste0("INSERT INTO orders (id, datesubmitted, itemdescription, submittedby, project, status, lastupdate) 
                   VALUES (",lastid+1,",'",format(Sys.Date(), format = "%d-%m-%Y"),"', '",itemDescription, "', '", from,
                              "', '",projectName,"', 'Requested', '",format(Sys.Date(), format = "%d-%m-%Y"),"');"))  
  } else {
    execute(paste0("INSERT INTO orders (id, datesubmitted, itemdescription, submittedby, project, status, lastupdate, images) 
                    VALUES (",lastid+1,",'",format(Sys.Date(), format = "%d-%m-%Y"),"', '",itemDescription, "', '", from,
                              "', '",projectName,"', 'Requested', '",format(Sys.Date(), format = "%d-%m-%Y"),"', '",drive_link,"');"))
  }
  orderid <- get_query("SELECT * FROM orders ORDER BY id DESC LIMIT 1")$id
  
  body_params <- list(
    list('type' = 'text', 'text' = from),
    list('type' = 'text', 'text' = projectName),
    list('type' = 'text', 'text' = orderid),
    list('type' = 'text', 'text' = itemDescription)
  )
  admin <- get_ap_wa()
  pm <- get_pm_wa()
  if (is.null(drive_link)) {
    sent_to_admin <- send_template(admin, body_params, "new_order", project = projectName, orderid = orderid)
    send_template(pm, body_params, "new_order_pm", project = projectName, orderid = orderid)
  } else {
    drive_auth(path = google_drive_service_acc)
    header <- list('type' = 'image', 'image' = list('id' = message_details$attachmentid))
    sent_to_admin <- send_template(admin, body_params, "new_order_image", heading = header ,project = projectName, orderid = orderid)
    send_template(pm, body_params, "new_order_pm_image", heading = header, project = projectName, orderid = orderid)
  }
  if (sent_to_admin) send_template(message_details$from, template_name = "order_received", project = projectName)
  else send_template(message_details$from, template_name = "order_failed", project = projectName)
}

task_QC_passed <- function(task_details) {
  execute(paste0("UPDATE tasks SET status = 'Completed' WHERE taskid = ", task_details$task, ";"))
  allstatuses <- get_query(paste0("SELECT status FROM tasks WHERE projectname = '", task_details$project, "';"))$status
  if (length(unique(allstatuses))==1 && unique(allstatuses) == "Completed") {
    body <- list(
      list("type" = "text", "text" = task_details$project)
    )
   QC <- get_pm_wa()
   send_template(QC, body, "all_tasks_completed", project = task_details$project)
   execute(paste0("UPDATE projects SET status = 'Ready for QC' WHERE projectname = '", task_details$project, "';"))
  }
}

tasks_ready_for_QC <- function(taskID) {
  df <- get_query(paste0("SELECT * FROM tasks WHERE taskid = '", taskID, "';"))
  execute(paste0("UPDATE tasks SET status = 'Ready for QC' WHERE taskid = ", taskID, ";"))
  body <- list(
    list("type" = "text", "text" =  df$employee), 
    list("type" = "text", "text" = paste0(df$taskname, ": ", df$description)), 
    list("type" = "text", "text" = df$projectname)
  )
  QC <- get_pm_wa()
  send_template(QC, body, "task_complete", taskid = taskID, project = df$projectname)
}

# check_reminders <- function() {
#   weekday <- weekdays(Sys.time()) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
#   current_hour <- as.numeric(format(Sys.time(), "%H"))
#   current_minute <- as.numeric(format(Sys.time(), "%M"))
#   is_between <- weekday && current_hour > 7 || (current_hour == 7 && current_minute >= 30) &&
#     current_hour < 15 || (current_hour == 15 && current_minute <= 30)
#   
#   if (is_between) {
#     print("Reminder check started")
#     not_responded <- get_query("SELECT * FROM sent_wa WHERE responded = 'false'")
#     if (nrow(not_responded) > 0) {
#       for (i in 1:nrow(not_responded)) {
#         if (not_responded$message[i] == 'task_time') {
#           taskid <- not_responded$task[i]
#           task_details <- get_query(paste0("SELECT * FROM tasks WHERE taskid = ", taskid, ";"))
#           if (as.POSIXct(task_details$plannedcompletion) < Sys.time()) {
#             sent_reminder <- get_query(paste0("SELECT * FROM sent_wa WHERE message = '",not_responded$id[i],"';")) %>% 
#               filter(timestamp > as.numeric(Sys.time()-ddays(1)))
#             if (nrow(sent_reminder) == 0) send_reminder(not_responded$recipientid[i], not_responded$id[i])
#           }
#         } else {
#           if (not_responded$timestamp[i] < as.numeric(Sys.time()-ddays(1))) {
#             sent_reminder <- get_query(paste0("SELECT * FROM sent_wa WHERE message = '",not_responded$id[i],"';")) %>% 
#               filter(timestamp > as.numeric(Sys.time()-ddays(1)))
#             if (nrow(sent_reminder) == 0) send_reminder(not_responded$recipientid[i], not_responded$id[i])
#           }
#         }
#       }
#     }
#     orders_not_received <- get_query(paste0("SELECT * FROM orders WHERE eta < '", Sys.Date(), "' AND status != 'Arrived';"))
#     if (nrow(orders_not_received)>0) {
#       for (i in 1: nrow (orders_not_received)) {
#         sent_reminder <- get_query(paste0("SELECT * FROM sent_wa WHERE message = 'purchase_not_received' AND orderid = '",orders_not_received$id[i],"';")) %>% 
#           filter(timestamp > as.numeric(Sys.time()-ddays(1)))
#         admin <- get_ap_wa()
#         if (nrow(sent_reminder) == 0) send_template(admin, list(list('type'='text', 'text'=as.character(orders_not_received$id[i]))),"purchase_not_received", project = orders_not_received$project[i], orderid = orders_not_received$id[i])
#       }
#       
#     }
#     print("Reminder check completed")
#   }
# }
# 
# send_reminder <- function(wa_id, context_id) {
#   url <- paste0(wa_api_url,wa_from,"/messages")
#   body <- list(
#     "messaging_product" = "whatsapp",
#     "to" = wa_id,
#     "context" = list(
#       "message_id" = context_id
#     ),
#     "type" = "text",
#     "text" = list(
#       "body" = "*Reminder:* no response has been received with regards to this message."
#     )
#   )
#   response <- POST(url, encode = "json", body=body, add_headers("Authorization" = paste0("Bearer ",wa_token), "Content-Type" = "application/json"))
#   
#   # Check the response status
#   status_code <- response$status_code
#   if (status_code %in% c(200, 201)) {
#     api_data <- content(response, "text", encoding = "UTF-8")
#     parsed_data <- fromJSON(api_data)
#     message_id <- fromJSON(rawToChar(response$content))[["messages"]][["id"]]
#     execute(paste0("INSERT INTO sent_wa (id, timestamp, recipientid, task, project, orderid, docid, invoicename, message, responded) VALUES ('",message_id 
#                    , "',", as.numeric(Sys.time()),
#                    ",'", wa_id, "','", NULL, "','", NULL, "','", NULL, "','", NULL,"', '",NULL,"','",context_id,"', 'true');"))
#     print(paste0("Sent reminder to ",wa_id, " regarding: ", context_id))
#     return(TRUE)
#   } else {
#     print(paste("Error: API request failed with status code", status_code, " REMINDER: ", context_id))
#     return(FALSE)
#   }
# }
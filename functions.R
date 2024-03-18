con <- reactiveVal(dbConnect(RPostgres::Postgres(), user = "u2tnmv2ufe7rpk", password = "p899046d336be15351280fd542015420a8e18e22dfe07c1cccaaa8e0e9fb20631", host = "cdgn4ufq38ipd0.cluster-czz5s0kz4scl.eu-west-1.rds.amazonaws.com", port = 5432, dbname = "d6qh1puq26hrth"))

get_query <- function(query) {
  a <- tryCatch({
    a <- dbGetQuery(con(), query)
  }, 
  error = function(e) {
    dbDisconnect(isolate(con())) 
    con(dbConnect(RPostgres::Postgres(), user = "u2tnmv2ufe7rpk", password = "p899046d336be15351280fd542015420a8e18e22dfe07c1cccaaa8e0e9fb20631", host = "cdgn4ufq38ipd0.cluster-czz5s0kz4scl.eu-west-1.rds.amazonaws.com", port = 5432, dbname = "d6qh1puq26hrth"))
    a <- dbGetQuery(con(), query)  
  })
  return(a)
}

execute <- function(query) {
  tryCatch({
    dbExecute(con(), query)
  }, 
  error = function(e) {
    dbDisconnect(isolate(con())) 
    con(dbConnect(RPostgres::Postgres(), user = "u2tnmv2ufe7rpk", password = "p899046d336be15351280fd542015420a8e18e22dfe07c1cccaaa8e0e9fb20631", host = "cdgn4ufq38ipd0.cluster-czz5s0kz4scl.eu-west-1.rds.amazonaws.com", port = 5432, dbname = "d6qh1puq26hrth"))
    dbExecute(con(), query)        
  })
}

get_pm_wa <- function() {return(get_query("SELECT wa_number FROM active_ts WHERE pm = 'true';")$wa_number)}

get_ac_wa <- function() {return(get_query("SELECT wa_number FROM active_ts WHERE ac = 'true';")$wa_number)}

get_ai_wa <- function() {return(get_query("SELECT wa_number FROM active_ts WHERE ai = 'true';")$wa_number)}

get_ap_wa <- function() {return(get_query("SELECT wa_number FROM active_ts WHERE ap = 'true';")$wa_number)}

get_md_wa <- function() {return(get_query("SELECT wa_number FROM active_ts WHERE md = 'true';")$wa_number)}

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

get_projects <- function(parsed_data) {
  extracted_data <- data.frame(
    ID = as.character(parsed_data$Results$ID),
    Name = parsed_data$Results$Name,
    Customer = parsed_data$Results$Customer$Name,
    CustomerID = parsed_data$Results$CustomerID,
    StartDate = parsed_data$Results$StartDate
  )
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

get_customers <- function() {
  parsed_data <- get_from_api("TimeTrackingCustomer", query_params = "$filter=Active eq true&")
  data.frame(
    CustomerID <- parsed_data$Results$ID,
    Customer <- parsed_data$Results$Name
  )
}

get_next_project <- function(projects) {
  next_p <- max(as.numeric(substr(projects()$Name, 1, 4))) + 1
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
  
  response <- POST(url, encode = "json", body=body, add_headers("Authorization" = paste0("Bearer ",wa_token), "Content-Type" = "application/json"))
  
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

get_new_webhooks <- function() {
  response <- GET(webhook_url)
  
  # Check the response status
  status_code <- response$status_code
  if (status_code == 200) {
    api_data <- content(response, "text", encoding = "UTF-8")
    parsed_data <- fromJSON(api_data)
    if (length(parsed_data$messages) > 0) {
      dbDisconnect(isolate(con())) 
      con(dbConnect(RPostgres::Postgres(), user = "u2tnmv2ufe7rpk", password = "p899046d336be15351280fd542015420a8e18e22dfe07c1cccaaa8e0e9fb20631", host = "cdgn4ufq38ipd0.cluster-czz5s0kz4scl.eu-west-1.rds.amazonaws.com", port = 5432, dbname = "d6qh1puq26hrth"))
      received_df <- data.frame()
      sent_df <- data.frame()
      for (webhook in parsed_data$messages$body$entry) {
        if (!is.null(webhook$changes[[1]]$value$messages[[1]])) {
          rec_webhook <- webhook$changes[[1]]$value$messages[[1]]
          type <- rec_webhook$type
          print(paste0("New ",type," webhook received from ", rec_webhook$from))
          from <- get_query(paste0("SELECT name FROM active_ts WHERE wa_number = '",rec_webhook$from,"';"))$name
          if (is.na(from)) {
            send_template(rec_webhook$from, template_name = "unknown_number")
            next
          }
          if (type == "button") {
            print(paste0("New" ,type," Webhook Received: ",rec_webhook$from, ": ", rec_webhook$button$text, 
                         ". Contextid = ",rec_webhook$context$id))
            df <- data.frame(
              "id" = rec_webhook$id,
              "contextid" =rec_webhook$context$id,
              "fromid" = rec_webhook$from,
              "timestamp" = rec_webhook$timestamp,
              "type" = type,
              "text" = rec_webhook$button$text,
              "attachmentid" = NA
            )
            handle_wa_button(df)
          } else if (type == "text") {
            print(paste0("New" ,type," Webhook Received: ",rec_webhook$from, ": ", rec_webhook$text$body, 
                         ". Contextid = ",rec_webhook$context$id))
            df <- data.frame(
              "id" = rec_webhook$id,
              "contextid" =ifelse (is.null(rec_webhook$context$id),NA, rec_webhook$context$id),
              "fromid" = rec_webhook$from,
              "timestamp" = rec_webhook$timestamp,
              "type" = type,
              "text" = gsub("\n", " ", rec_webhook$text$body),
              "attachmentid" = NA
            )
            if (grepl("^N[0-9]{4}.*$", df$text)) { 
              add_note(df)
            } else if (grepl("^INV\\d{5}$", df$text)) {
              send_invoice_to_AC(df)
            } else if (grepl("^P[0-9]{4}.*$", df$text)) {
              create_order(df)
            } else send_template(rec_webhook$from, template_name = "unknown_request")
            
          } else if (type == "image") {
            print(paste0("New" ,type," Webhook Received: ",rec_webhook$from, ": ", rec_webhook$image$caption, 
                         ". Contextid = ",rec_webhook$context$id))
            df <- data.frame(
              "id" = rec_webhook$id,
              "contextid" =ifelse (is.null(rec_webhook$context$id),NA, rec_webhook$context$id),
              "fromid" = rec_webhook$from,
              "timestamp" = rec_webhook$timestamp,
              "type" = type,
              "text" = ifelse (is.null(rec_webhook$image$caption),NA, gsub("\n", " ", rec_webhook$image$caption)),
              "attachmentid" = rec_webhook$image$id
            )
            if (grepl("^[0-9]{4}$", df$text)) {
              drive_link <- get_WA_image_and_upload(df, "ProjectImages", df$text)
              execute(paste0("UPDATE projects SET images = '",drive_link, "' WHERE projectname LIKE '",df$text,"%';" ))
              forward_image(df, from)
            }
            else if (grepl("^P[0-9]{4}.*$", df$text)) {
              drive_link <- get_WA_image_and_upload(df, "ProjectOrders", substr(df$text, 2,5))
              create_order(df, drive_link)
            } else send_template(rec_webhook$from, template_name = "unknown_request")
          } else if (type == "document") {
            print(paste0("New" ,type," webhook received: ",rec_webhook$from, ": ", rec_webhook$document$caption, 
                         ". Contextid = ",rec_webhook$context$id))
            df <- data.frame(
              "id" = rec_webhook$id,
              "contextid" =ifelse (is.null(rec_webhook$context$id),NA, rec_webhook$context$id),
              "fromid" = rec_webhook$from,
              "timestamp" = rec_webhook$timestamp,
              "type" = type,
              "text" = ifelse(is.null(rec_webhook$document$caption),NA, gsub("\n", " ", rec_webhook$document$caption)),
              "attachmentid" = rec_webhook$document$id
            )
            handle_wa_button(df)
          }
          else {
            send_template(rec_webhook$from, template_name = "unknown_request")
          }
          received_df <- bind_rows(received_df, df)
        }
      }
      if (nrow(received_df) > 0) {
        unique_df <- received_df[!duplicated(received_df), ]
        for (i in 1:nrow(unique_df)) {
          row <- unique_df[i, ]
          execute(paste0("INSERT INTO received_wa (id, fromid, timestamp, type, text) VALUES ('",
                                    row$id, "', '", row$fromid, "', ", row$timestamp,", '", row$type,"', '", row$text,"');"))        
          print(paste0("Webhook processed: ", row$text, " FROM ", row$fromid))
        }
      }
    }
  } 
}

get_WA_image_and_upload <- function(df, folder_name, project_name) {
  url <- paste0(wa_api_url, df$attachmentid, "?","phone_number_id=", wa_from)
  response <- GET(url, add_headers("Authorization" = paste0("Bearer ",wa_token), "Content-Type" = "application/json"))
  
  # Check the response status
  status_code <- response$status_code
  if (status_code == 200) {
    api_data <- content(response, "text", encoding = "UTF-8")
    parsed_data <- fromJSON(api_data)
    
    image_url <- parsed_data$url
    
    response2 <- GET(image_url, add_headers(Authorization = paste("Bearer", wa_token)),
                     user_agent("Mozilla/5.0"))
    filename <- paste0(
      trimws(substr(df$text,7,nchar(df$text)), "left"),
      "_",
      paste0(sample(c(0:9, letters, LETTERS), 5, replace = TRUE), collapse = ""),
      ".jpg"
    )
    # Check if the request was successful (status code 200)
    if (response2$status_code == 200) {
      uploadToGoogleDrive(response2$content, project_name, folder_name, filename)
    }
  } 
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

add_note <- function(df) {
  projNumber <- substr(df$text, 2, 5)
  note <- trimws(substr(df$text, 7, nchar(df$text)), "left") 
  from <- (get_query(paste0("SELECT name FROM active_ts WHERE wa_number = '",df$from,"';"))$name)[1]
  projectName <- get_query(paste0("SELECT projectname FROM projects WHERE projectname LIKE '", projNumber, "%';"))$projectname
  existingNotes <- get_query(paste0("SELECT notes FROM projects WHERE projectname = '", projectName, "';"))$notes
  if (!is.na(existingNotes)) {note = paste0(existingNotes, "   ", note)}
  
  execute(paste0("UPDATE projects SET notes = '",note,"' WHERE projectname = '",projectName,"';"))
  
  body_params <- list(
    list('type' = 'text', 'text' = from),
    list('type' = 'text', 'text' = projectName),
    list('type' = 'text', 'text' = note)
  )
  pm <- get_pm_wa()
  if (is.na(from) || pm != from) sent_to_pm <- send_template(pm, body_params, "new_note", project = projectName)
  else sent_to_pm <- TRUE
  
  
  if (sent_to_pm) send_template(df$from, template_name = "note_success", project = projectName)
  else send_template(df$from, template_name = "note_failed", project = projectName)
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

handle_wa_button <- function(button_details, invoiceName=NULL) {
  if (button_details$type == "document") {
    accounts <- get_ac_wa()
    if (accounts == button_details$from) {
      sent_wa <- get_query(paste0("SELECT * FROM sent_wa WHERE id = '", button_details$contextid, "';"))
      if (!is.null(sent_wa$docid)) {
        docID <- sent_wa$docid
        orderid <- sent_wa$orderid
        order_description <- get_query(paste0("SELECT itemdescription FROM orders WHERE id = ",orderid,";"))$itemdescription
        header1 <- list(
          'type' = 'document',
          'document' = list(
            'id' = docID
          )
        )
        header2 <- list(
          'type' = 'document',
          'document' = list(
            'id' = button_details$attachmentid
          )
        )
        body_params1 <- list(
          list('type' = 'text', 'text' = as.character(sent_wa$orderid)),
          list('type' = 'text', 'text' = order_description)
        )
        body_params2 <- list(
          list('type' = 'text', 'text' = as.character(sent_wa$orderid))
        )
        admin <- get_ai_wa()
        send_template(admin, body_params1, "quote_pmt_1", header1, project=sent_wa$project, orderid = orderid, docid = docID)
        send_template(admin, body_params2, "quote_pmt_2", header2, project=sent_wa$project, orderid = orderid, docid = button_details$attachmentid)
        execute(paste0("UPDATE orders SET status = 'Paid', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),"' WHERE id = ", orderid, ";"))
      }
    } else send_template(button_details$from, template_name = "unknown_request")
  }
  
  else if (!is.na(button_details$text)) {
    
    if (button_details$text == "Task Completed") {
      execute(paste0("UPDATE sent_wa SET responded = 'true' WHERE id = '", button_details$contextid, "';"))
      tasks_ready_for_QC(get_query(paste0("SELECT task FROM sent_wa WHERE id = '", button_details$contextid, "';"))$task)
    }
    
    else if (button_details$text == "QC Passed!") {
      execute(paste0("UPDATE sent_wa SET responded = 'true' WHERE id = '", button_details$contextid, "';"))
      task_QC_passed(get_query(paste0("SELECT * FROM sent_wa WHERE id = '", button_details$contextid, "';")))
    }
    
    else if (button_details$text == "Approve Quote") {
      execute(paste0("UPDATE sent_wa SET responded = 'true' WHERE id = '", button_details$contextid, "';"))
      sent_wa <- get_query(paste0("SELECT * FROM sent_wa WHERE id = '",button_details$contextid,"';"))
      docID <- sent_wa$docid
      orderid <- sent_wa$orderid
      order_description <- get_query(paste0("SELECT itemdescription FROM orders WHERE id = ",orderid,";"))$itemdescription
      header <- list(
        'type' = 'document',
        'document' = list(
          'id' = docID
        )
      )
      body_params <- list(
        list('type' = 'text', 'text' = sent_wa$project),
        list('type' = 'text', 'text' = as.character(orderid)),
        list('type' = 'text', 'text' = order_description)
      )
      accounts <- get_ac_wa()
      send_template(accounts, body_params, "request_quote_pmt", header, project=sent_wa$project, orderid = orderid, docid = docID)
      execute(paste0("UPDATE orders SET status = 'Quote Approved', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),"' WHERE id = ", orderid, ";"))
    }
    
    else if (button_details$text == "Proceed Without Payment") {
      execute(paste0("UPDATE sent_wa SET responded = 'true' WHERE id = '", button_details$contextid, "';"))
      sent_wa <- get_query(paste0("SELECT * FROM sent_wa WHERE id = '",button_details$contextid,"';"))
      docID <- sent_wa$docid
      orderid <- sent_wa$orderid
      order_description <- get_query(paste0("SELECT itemdescription FROM orders WHERE id = ",orderid,";"))$itemdescription
      header <- list(
        'type' = 'document',
        'document' = list(
          'id' = docID
        )
      )
      body_params <- list(
        list('type' = 'text', 'text' = as.character(sent_wa$orderid)),
        list('type' = 'text', 'text' = order_description)
      )
      admin <- get_ai_wa()
      send_template(admin, body_params, "quote_no_pmt", header, project=sent_wa$project, orderid = orderid, docid = docID)
      execute(paste0("UPDATE orders SET status = 'Order Go Ahead', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),"' WHERE id = ", orderid, ";"))
    }
    
    else if (button_details$text == "Approve Purchase") {
      execute(paste0("UPDATE sent_wa SET responded = 'true' WHERE id = '", button_details$contextid, "';"))
      sent_wa <- get_query(paste0("SELECT * FROM sent_wa WHERE id = '",button_details$contextid,"';"))
      orderid <- sent_wa$orderid
      order_description <- get_query(paste0("SELECT itemdescription FROM orders WHERE id = ",orderid,";"))$itemdescription
      body_params <- list(
        list('type' = 'text', 'text' = sent_wa$project),
        list('type' = 'text', 'text' = as.character(sent_wa$orderid)),
        list('type' = 'text', 'text' = order_description)
      )
      admin <- get_ap_wa()
      send_template(admin, body_params, "purchase_approval", project=sent_wa$project, orderid = orderid)
      execute(paste0("UPDATE orders SET status = 'Purchase Go Ahead', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),"' WHERE id = ", orderid, ";"))
    }
    
    else if (button_details$text == "Complete Project") {
      execute(paste0("UPDATE sent_wa SET responded = 'true' WHERE id = '", button_details$contextid, "';"))
      projectName <- get_query(paste0("SELECT project FROM sent_wa WHERE id = '", button_details$contextid, "';"))$project
      execute(paste0("UPDATE projects SET status = 'To be Invoiced', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                                "' WHERE projectname = '", projectName, "';"))
      
      body <- list(
        list("type" = "text", "text" = projectName)
      )
      admin <- get_ai_wa()
      send_template(admin, body, "start_invoice", project = projectName)
      md <- get_md_wa()
      send_template(md, body, template_name = 'md_project_completed', project = projectName)
    }
    
    else if (button_details$text == "Approve Invoice") {
      execute(paste0("UPDATE sent_wa SET responded = 'true' WHERE id = '", button_details$contextid, "';"))
      sent_WA <- get_query(paste0("SELECT * FROM sent_wa WHERE id = '",button_details$contextid,"';"))
      header <- list(
        'type' = 'document',
        'document' = list(
          'id' = sent_WA$docid,
          'filename' = paste0(sent_WA$invoicename, ".pdf")
        )
      )
      if (button_details$fromid == get_ac_wa()) {
        body_params <- list(list('type' = 'text', 'text' = sent_WA$project))
        pm <- get_pm_wa()
        send_template(pm, body_params, "invoice_approval", heading = header, project = sent_WA$project, docid = sent_WA$docid, invoicename = sent_WA$invoicename)
        execute(paste0("UPDATE projects SET status = 'Invoice Sent to Deneys', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                                  "' WHERE projectname = '", sent_WA$project, "';"))
      } else if (button_details$fromid == get_pm_wa()) {
        body_params <- list(list('type' = 'text', 'text' = sent_WA$project))
        main_director <- get_query(paste0("SELECT wa_number FROM active_ts WHERE md = 'true';"))$wa_number
        send_template(main_director, body_params, "invoice_approval", heading = header, project = sent_WA$project, docid = sent_WA$docid, invoicename = sent_WA$invoicename)
        execute(paste0("UPDATE projects SET status = 'Invoice Sent to Sven', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                                  "' WHERE projectname = '", sent_WA$project, "';")) 
      } else {
        invoice_data <- (get_from_api("TaxInvoice", query_params = paste0("$filter=DocumentNumber eq '",sent_WA$invoicename,"'&includeDetail=False&includeCustomerDetails=True&") 
        ))
        admin <- get_ai_wa()
        if (invoice_data$TotalResults > 0) {
          invoiceID <- invoice_data$Results$ID
          if (is.null(invoice_data$Results$Customer$Mobile) || invoice_data$Results$Customer$Mobile == "") {
            body_params <- list(
              list('type' = 'text', 'text' = sent_WA$invoicename),
              list('type' = 'text', 'text' = "The customer's mobile number is missing.")
            )
            send_template(admin, template_name = "invoice_failed", project = sent_WA$project, body_params = body_params, invoicename = sent_WA$invoicename)
          } else {
            customerMobile <- get_WA_number(invoice_data$Results$Customer$Mobile)
            customerName <- invoice_data$Results$Customer$ContactName
            if (customerName == "") customerName <- " "
            body_params <- list(list('type' = 'text', 'text' = customerName))
            success <- send_template(customerMobile, body_params, "invoice", header, project=sent_WA$project, invoicename = sent_WA$invoicename)
            if (success) send_template(admin, body_params = body_params, template_name = "invoice_success", project = sent_WA$project, invoicename = sent_WA$invoicename)
            else {
              body_params <- list(list('type' = 'text', 'text' = customerName),
                                  list('type' = 'text', 'text' = "An error occurred while trying to send the invoice to the customer."))
              send_template(admin, template_name = "invoice_failed", project = sent_WA$project, body_params = body_params, invoicename = sent_WA$invoicename)
            }
          }
        } else {
          body_params <- list(list('type' = 'text', 'text' = sent_WA$invoicename),
                              list('type' = 'text', 'text' = "Unable to find the invoice on Sage."))
          send_template(admin, body_params = body_params, template_name = "invoice_failed", project = sent_WA$project, body_params = body_params, invoicename = sent_WA$invoicename)
        }
        execute(paste0("UPDATE projects SET status = 'Invoiced', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                                "' WHERE projectname = '", sent_WA$project, "';"))
      }
    } else send_template(button_details$from, template_name = "unknown_request")
  } else send_template(button_details$from, template_name = "unknown_request")
}

get_WA_number <- function(number) {
  wa_number <- paste0("27", substr(number, 2, nchar(number)))
  return(wa_number)
}

sync_invoices_and_projects <- function() {
  print("Start of invoices sync")
  invoice_data <- (get_from_api("TaxInvoice", query_params = paste0("includeDetail=False&includeCustomerDetails=False&") ))
  
  if (invoice_data$TotalResults > 0) {
    invoices <- data.frame(
      "project" = substr(invoice_data$Results$Reference,1, 4),
      "invoiceno" = invoice_data$Results$DocumentNumber,
      "invoice_status" = invoice_data$Results$Status,
      "customer" = invoice_data$Results$CustomerName
    )
    projects <- get_query("SELECT projectname, customer, invoiceno, invoice_status FROM projects;") %>%
      mutate("project" = substr(projectname, 1,4))
    merged_projects <- merge(projects, invoices, by = c("project", "customer"), all.x = TRUE) %>%
      filter((is.na(invoiceno.x) & !is.na(invoiceno.y)) | (is.na(invoice_status.x) & !is.na(invoice_status.y)) | invoiceno.y != invoiceno.x | invoice_status.x != invoice_status.y)
    if (nrow(merged_projects)>0) {
      for (i in 1:nrow(merged_projects)) {
        execute(paste0("UPDATE projects SET invoiceno = '", merged_projects$invoiceno.y[i], "', invoice_status = '", merged_projects$invoice_status.y[i], "' WHERE projectname = '",merged_projects$projectname[i],"';"))
        pm <- get_pm_wa()
        md <- get_md_wa()
        body <- list(
          list('type'='text', 'text'=merged_projects$invoiceno.y[i]),
          list('type'='text', 'text'=merged_projects$projectname[i]),
          list('type'='text', 'text'=merged_projects$invoice_status.y[i])
        )
        send_template(pm, body, 'project_invoice_update', project = merged_projects$projectname[i], invoicename = merged_projects$invoiceno.y[i])
        send_template(md, body, 'project_invoice_update', project = merged_projects$projectname[i], invoicename = merged_projects$invoiceno.y[i])
      }
    }
  }
  if (!is.null(merged_projects)) n <- nrow(merged_projects)
  else n <- 0
  print(paste0("End of invoices sync: ", n))
}

send_invoice_to_AC <- function(message_details) {
  execute(paste0("UPDATE sent_wa SET responded = 'true' WHERE id = '", message_details$contextid, "';"))
  projectName <- get_query(paste0("SELECT project FROM sent_wa WHERE id = '", message_details$contextid, "';"))$project
  invoice_data <- (get_from_api(
    "TaxInvoice", 
    query_params = paste0("$filter=DocumentNumber eq '",message_details$text,"'&includeDetail=False&includeCustomerDetails=False&") 
  ))
  if (invoice_data$TotalResults > 0) {
    invoiceID <- invoice_data$Results$ID
    
    # Get document
    url <- paste0(api_url, "TaxInvoice/Export/", invoiceID, "?companyid=", companyid, "&apikey=", apikey)
    response <- GET(url, authenticate(username, password))
    status_code <- response$status_code
    if (status_code == 200) {
      temp_file <- tempfile(fileext = ".pdf")
      writeBin(response$content, temp_file)
      filename <- paste0(message_details$text,".pdf")
      
      # Upload doc to Whatsapp
      body = list(
        'messaging_product' = 'whatsapp',
        'file' = upload_file(temp_file),
        'name' = filename
      )
      res <- VERB("POST", url = "https://graph.facebook.com/v18.0/206349329226876/media", body = body, add_headers("Authorization" = paste0("Bearer ",wa_token)), encode = 'multipart')
      unlink(temp_file)
      docID <- fromJSON(content(res, 'text'))$id
      
      # Send invoice to PM
      header <- list(
        'type' = 'document',
        'document' = list(
          'id' = docID,
          'filename' = filename
        )
      )
      
      body_params <- list(list('type' = 'text', 'text' = projectName))
      ac <- get_ac_wa()
      send_template(ac, body_params, "invoice_approval", heading = header, project = projectName, docid = docID, invoicename = message_details$text)
    }} else {
      body_params <- list(list('type' = 'text', 'text' = message_details$text),
                          list('type' = 'text', 'text' = "Unable to find the invoice on Sage."))
      admin <- get_ai_wa()
      send_template(admin, template_name = "invoice_failed", project = projectName, body_params = body_params, invoicename = message_details$text)
    }
  execute(paste0("UPDATE projects SET status = 'Invoice Sent to PM', invoiceno = '",message_details$text,"', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                            "' WHERE projectname = '", projectName, "';"))
}

send_invoice_to_customer <- function(invoiceNo, invoiceID, wa_id, customerName, projectName) {
  # Get document
  url <- paste0(api_url, "TaxInvoice/Export/", invoiceID, "?companyid=", companyid, "&apikey=", apikey)
  response <- GET(url, authenticate(username, password))
  status_code <- response$status_code
  if (status_code == 200) {
    temp_image <- tempfile(fileext = ".pdf")
    writeBin(response$content, temp_image)
    filename <- paste0(invoiceNo,".pdf")
    
    # Upload doc to Whatsapp
    body = list(
      'messaging_product' = 'whatsapp',
      'file' = upload_file(temp_image),
      'name' = filename
    )
    res <- VERB("POST", url = "https://graph.facebook.com/v18.0/206349329226876/media", body = body, add_headers("Authorization" = paste0("Bearer ",wa_token)), encode = 'multipart')
    unlink(temp_image)
    docID <- fromJSON(content(res, 'text'))$id
    
    # Send invoice to Customer
    header <- 
      list(
        'type' = 'document',
        'document' = list(
          'id' = docID,
          'filename' = filename
        )
        
      )
    body_params <- list(
      list('type' = 'text', 'text' = customerName)
    )
    
    message <- send_template(wa_id, body_params, "invoice", header, project=projectName)
    return(message)
  } else return(FALSE)
}

get_active_projects <- function() {      
  active_proj <- get_query(paste0("SELECT projectid FROM projects WHERE status IN ('Not Started', 'In Progress');"))$projectid
  projects <- get_projects(get_from_api("TimeTrackingProject","GetActiveProjects")) %>% filter(ID %in% active_proj)
}

forward_image <- function(df, from) {
  pm <- get_pm_wa()
  md <- get_md_wa()
  header <- list('type' = 'image', 'image' = list('id' = df$attachmentid))
  projectName <- df$text
  body <- list(
    list('type' = 'text', 'text' = from),
    list('type' = 'text', 'text' = projectName)
  )
  send_template(pm, body, "project_image_fwd", heading = header, project = projectName)
  send_template(md, body, "project_image_fwd", heading = header ,project = projectName)
}
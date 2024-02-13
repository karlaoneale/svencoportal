con <- reactiveVal(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))


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
  if (!is.null(heading)) h <- list("type" = "header","parameters" = list(heading))
  if (!is.null(body_params)) b <- list("type" = "body","parameters" = body_params)
  
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
    tryCatch({
      dbExecute(con(),paste0("INSERT INTO sent_wa (id, timestamp, recipientid, task, project, orderid, docid, invoicename, message) VALUES ('",message_id 
                             , "',", as.numeric(Sys.time()),
                             ",'", wa_id, "','", taskid, "','", project, "','", orderid, "','", docid,"', '",invoicename,"','",template_name,"');"))
    }, 
    error = function(e) {
      con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
      dbExecute(con(),paste0("INSERT INTO sent_wa (id, timestamp, recipientid, task, project, orderid, docid, invoicename, message) VALUES ('",message_id 
                             , "',", as.numeric(Sys.time()),
                             ",'", wa_id, "','", taskid, "','", project, "','", orderid, "','", docid,"', '",invoicename,"','",template_name,"');"))
    })
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
      con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
      received_df <- data.frame()
      sent_df <- data.frame()
      for (webhook in parsed_data$messages$body$entry) {
        if (!is.null(webhook$changes[[1]]$value$messages[[1]])) {
          rec_webhook <- webhook$changes[[1]]$value$messages[[1]]
          type <- rec_webhook$type
          print(paste0("New ",type," webhook received from ", rec_webhook$from))
          from <- tryCatch({
            from <- (dbGetQuery(con(), paste0("SELECT name FROM active_ts WHERE wa_number = '",rec_webhook$from,"';"))$name)[1]
          }, 
          error = function(e) {
            con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
            from <- (dbGetQuery(con(), paste0("SELECT name FROM active_ts WHERE wa_number = '",rec_webhook$from,"';"))$name)[1]
          })
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
              "text" = rec_webhook$text$body,
              "attachmentid" = NA
            )
            if (grepl("^N[0-9]{4}.*$", df$text)) { 
              add_note(df)
            } else if (grepl("^INV\\d{6}$", df$text)) {
              send_invoice_to_PM(df)
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
              "text" = ifelse (is.null(rec_webhook$image$caption),NA, rec_webhook$image$caption),
              "attachmentid" = rec_webhook$image$id
            )
            if (grepl("^[0-9]{4}$", df$text)) {
              drive_link <- get_WA_image_and_upload(df, "ProjectImages", df$text)
              tryCatch({
                dbExecute(con(), paste0("UPDATE projects SET images = '",drive_link, "' WHERE projectname LIKE '",df$text,"%';" ))
              }, 
              error = function(e) {
                con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
                dbExecute(con(), paste0("UPDATE projects SET images = '",drive_link, "' WHERE projectname LIKE '",df$text,"%';" ))
              })
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
              "text" = ifelse(is.null(rec_webhook$document$caption),NA, rec_webhook$document$caption),
              "attachmentid" = rec_webhook$document$id
            )
            handle_wa_button(df)
          }
          received_df <- bind_rows(received_df, df)
        }
      }
      if (nrow(received_df) > 0) {
        unique_df <- received_df[!duplicated(received_df), ]
        for (i in 1:nrow(unique_df)) {
          row <- unique_df[i, ]
          tryCatch({
            dbExecute(con(), paste0("INSERT INTO received_wa (id, fromid, timestamp, type, text) VALUES ('",
                                    row$id, "', '", row$fromid, "', ", row$timestamp,", '", row$type,"', '", row$text,"');"))          }, 
          error = function(e) {
            con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
            dbExecute(con(), paste0("INSERT INTO received_wa (id, fromid, timestamp, type, text) VALUES ('",
                                    row$id, "', '", row$fromid, "', ", row$timestamp,", '", row$type,"', '", row$text,"');"))          })
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
      substr(df$text,7,nchar(df$text)),
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
  projNumber <- substr(message_details$text, 2, 5)
  itemDescription <- substr(message_details$text, 7, nchar(message_details$text))
  from <- tryCatch({
    from <- (dbGetQuery(con(), paste0("SELECT name FROM active_ts WHERE wa_number = '",message_details$from,"';"))$name)[1]
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    from <- (dbGetQuery(con(), paste0("SELECT name FROM active_ts WHERE wa_number = '",message_details$from,"';"))$name)[1]
  })
  projectName <- tryCatch({
    projectName <- dbGetQuery(con(), paste0("SELECT projectname FROM projects WHERE projectname LIKE '", projNumber, "%';"))$projectname
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    projectName <- dbGetQuery(con(), paste0("SELECT projectname FROM projects WHERE projectname LIKE '", projNumber, "%';"))$projectname
  })
  
  if (is.null(drive_link)) {
    tryCatch({
      dbExecute(con(), paste0("INSERT INTO orders (datesubmitted, itemdescription, submittedby, project, status, lastupdate) VALUES ('",format(Sys.Date(), format = "%d-%m-%Y"),"', '",itemDescription, "', '", from,
                              "', '",projectName,"', 'Requested', '",format(Sys.Date(), format = "%d-%m-%Y"),"') RETURNING id;"))    }, 
    error = function(e) {
      con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
      dbExecute(con(), paste0("INSERT INTO orders (datesubmitted, itemdescription, submittedby, project, status, lastupdate) VALUES ('",format(Sys.Date(), format = "%d-%m-%Y"),"', '",itemDescription, "', '", from,
                              "', '",projectName,"', 'Requested', '",format(Sys.Date(), format = "%d-%m-%Y"),"') RETURNING id;"))    })
  } else {
    tryCatch({
      dbExecute(con(), paste0("INSERT INTO orders (datesubmitted, itemdescription, submittedby, project, status, lastupdate, images) 
                            VALUES ('",format(Sys.Date(), format = "%d-%m-%Y"),"', '",itemDescription, "', '", from,
                              "', '",projectName,"', 'Requested', '",format(Sys.Date(), format = "%d-%m-%Y"),"', '",drive_link,"') RETURNING id;"))    }, 
    error = function(e) {
      con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
      dbExecute(con(), paste0("INSERT INTO orders (datesubmitted, itemdescription, submittedby, project, status, lastupdate, images) 
                            VALUES ('",format(Sys.Date(), format = "%d-%m-%Y"),"', '",itemDescription, "', '", from,
                              "', '",projectName,"', 'Requested', '",format(Sys.Date(), format = "%d-%m-%Y"),"', '",drive_link,"') RETURNING id;"))    })
  }
  orderid <- tryCatch({
    orderid <- dbGetQuery(con(), "SELECT * FROM orders ORDER BY id DESC LIMIT 1")$id
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    orderid <- dbGetQuery(con(), "SELECT * FROM orders ORDER BY id DESC LIMIT 1")$id
  })
  
  body_params <- list(
    list('type' = 'text', 'text' = from),
    list('type' = 'text', 'text' = projectName),
    list('type' = 'text', 'text' = orderid),
    list('type' = 'text', 'text' = itemDescription)
  )
  admin <- tryCatch({
    admin <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ap = 'true';"))$wa_number
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    admin <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ap = 'true';"))$wa_number
  })
  pm <- tryCatch({
    pm <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE pm = 'true';"))$wa_number
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    pm <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE pm = 'true';"))$wa_number
  })
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
  note <- substr(df$text, 7, nchar(df$text))
  from <- tryCatch({
    from <- (dbGetQuery(con(), paste0("SELECT name FROM active_ts WHERE wa_number = '",df$from,"';"))$name)[1]
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    from <- (dbGetQuery(con(), paste0("SELECT name FROM active_ts WHERE wa_number = '",df$from,"';"))$name)[1]
  })
  projectName <- tryCatch({
    projectName <- dbGetQuery(con(), paste0("SELECT projectname FROM projects WHERE projectname LIKE '", projNumber, "%';"))$projectname
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    projectName <- dbGetQuery(con(), paste0("SELECT projectname FROM projects WHERE projectname LIKE '", projNumber, "%';"))$projectname
  })
  existingNotes <- tryCatch({
    existingNotes <- dbGetQuery(con(), paste0("SELECT notes FROM projects WHERE projectname = '", projectName, "';"))$notes
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    existingNotes <- dbGetQuery(con(), paste0("SELECT notes FROM projects WHERE projectname = '", projectName, "';"))$notes
  })
  if (!is.na(existingNotes)) {note = paste0(existingNotes, "   ", note)}
  
  tryCatch({
    dbExecute(con(), paste0("UPDATE projects SET notes = '",note,"' WHERE projectname = '",projectName,"';"))
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    dbExecute(con(), paste0("UPDATE projects SET notes = '",note,"' WHERE projectname = '",projectName,"';"))
  })
  
  body_params <- list(
    list('type' = 'text', 'text' = from),
    list('type' = 'text', 'text' = projectName),
    list('type' = 'text', 'text' = note)
  )
  pm <- tryCatch({
    pm <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE pm = 'true';"))$wa_number
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    pm <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE pm = 'true';"))$wa_number
  })
  if (is.na(from) || pm != from) sent_to_pm <- send_template(pm, body_params, "new_note", project = projectName)
  else sent_to_pm <- TRUE
  
  
  if (sent_to_pm) send_template(df$from, template_name = "note_success", project = projectName)
  else send_template(df$from, template_name = "note_failed", project = projectName)
}

task_QC_passed <- function(task_details) {
  tryCatch({
    dbExecute(con(), paste0("UPDATE tasks SET status = 'Completed' WHERE taskid = ", task_details$task, ";"))
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    dbExecute(con(), paste0("UPDATE tasks SET status = 'Completed' WHERE taskid = ", task_details$task, ";"))
  })
  allstatuses <- tryCatch({
    allstatuses <- dbGetQuery(con(), paste0("SELECT status FROM tasks WHERE projectname = '", task_details$project, "';"))$status
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    allstatuses <- dbGetQuery(con(), paste0("SELECT status FROM tasks WHERE projectname = '", task_details$project, "';"))$status
  })
  if (unique(allstatuses) == "Completed") {
    body <- list(
      list("type" = "text", "text" = task_details$project)
    )
    QC <- tryCatch({
      QC <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE pm = 'true';"))$wa_number
    }, 
    error = function(e) {
      con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
      QC <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE pm = 'true';"))$wa_number
    })
    send_template(QC, body, "all_tasks_completed", project = task_details$project)
    tryCatch({
      dbExecute(con(), paste0("UPDATE projects SET status = 'Ready for QC' WHERE projectname = '", task_details$project, "';"))
    }, 
    error = function(e) {
      con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
      dbExecute(con(), paste0("UPDATE projects SET status = 'Ready for QC' WHERE projectname = '", task_details$project, "';"))
    })
  }
}

tasks_ready_for_QC <- function(taskID) {
  df <- tryCatch({
    df <- dbGetQuery(con(), paste0("SELECT * FROM tasks WHERE taskid = '", taskID, "';"))
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    df <- dbGetQuery(con(), paste0("SELECT * FROM tasks WHERE taskid = '", taskID, "';"))
  })
  tryCatch({
    dbExecute(con(), paste0("UPDATE tasks SET status = 'Ready for QC' WHERE taskid = ", taskID, ";"))
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    dbExecute(con(), paste0("UPDATE tasks SET status = 'Ready for QC' WHERE taskid = ", taskID, ";"))
  })
  body <- list(
    list("type" = "text", "text" =  df$employee), 
    list("type" = "text", "text" = paste0(df$taskname, ": ", df$description)), 
    list("type" = "text", "text" = df$projectname)
  )
  QC <- tryCatch({
    QC <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE pm = 'true';"))$wa_number
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    QC <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE pm = 'true';"))$wa_number
  })
  send_template(QC, body, "task_complete", taskid = taskID, project = df$projectname)
}

handle_wa_button <- function(button_details, invoiceName=NULL) {
  if (button_details$type == "document") {
    accounts <- tryCatch({
      accounts <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ac = 'true';"))$wa_number    }, 
    error = function(e) {
      con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
      accounts <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ac = 'true';"))$wa_number    })
    if (accounts == button_details$from) {
      sent_wa <- tryCatch({
        sent_wa <- dbGetQuery(con(), paste0("SELECT * FROM sent_wa WHERE id = '", button_details$contextid, "';"))
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        sent_wa <- dbGetQuery(con(), paste0("SELECT * FROM sent_wa WHERE id = '", button_details$contextid, "';"))
      })
      if (!is.null(sent_wa$docid)) {
        docID <- sent_wa$docid
        orderid <- sent_wa$orderid
        order_description <- tryCatch({
          order_description <- dbGetQuery(con(), paste0("SELECT itemdescription FROM orders WHERE id = ",orderid,";"))$itemdescription
        }, 
        error = function(e) {
          con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
          order_description <- dbGetQuery(con(), paste0("SELECT itemdescription FROM orders WHERE id = ",orderid,";"))$itemdescription
        })
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
        admin <- tryCatch({
          admin <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ai = 'true';"))$wa_number
        }, 
        error = function(e) {
          con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
          admin <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ai = 'true';"))$wa_number
        })
        send_template(admin, body_params1, "quote_pmt_1", header1, project=sent_wa$project, orderid = orderid, docid = docID)
        send_template(admin, body_params2, "quote_pmt_2", header2, project=sent_wa$project, orderid = orderid, docid = button_details$attachmentid)
        tryCatch({
          dbExecute(con(), paste0("UPDATE orders SET status = 'Paid', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),"' WHERE id = ", orderid, ";"))
        }, 
        error = function(e) {
          con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
          dbExecute(con(), paste0("UPDATE orders SET status = 'Paid', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),"' WHERE id = ", orderid, ";"))
        })
      }
    } else send_template(button_details$from, template_name = "unknown_request")
  }
  
  else if (!is.na(button_details$text)) {
    
    if (button_details$text == "Task Completed") {
      tryCatch({
        tasks_ready_for_QC(dbGetQuery(con(), paste0("SELECT task FROM sent_wa WHERE id = '", button_details$contextid, "';"))$task)
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        tasks_ready_for_QC(dbGetQuery(con(), paste0("SELECT task FROM sent_wa WHERE id = '", button_details$contextid, "';"))$task)
      })
    }
    
    else if (button_details$text == "QC Passed!") {
      tryCatch({
        task_QC_passed(dbGetQuery(con(), paste0("SELECT * FROM sent_wa WHERE id = '", button_details$contextid, "';")))
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        task_QC_passed(dbGetQuery(con(), paste0("SELECT * FROM sent_wa WHERE id = '", button_details$contextid, "';")))
      })
    }
    
    else if (button_details$text == "Approve Quote") {
      sent_wa <- tryCatch({
        sent_wa <- dbGetQuery(con(), paste0("SELECT * FROM sent_wa WHERE id = '",button_details$contextid,"';"))
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        sent_wa <- dbGetQuery(con(), paste0("SELECT * FROM sent_wa WHERE id = '",button_details$contextid,"';"))
      })
      docID <- sent_wa$docid
      orderid <- sent_wa$orderid
      order_description <- tryCatch({
        order_description <- dbGetQuery(con(), paste0("SELECT itemdescription FROM orders WHERE id = ",orderid,";"))$itemdescription
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        order_description <- dbGetQuery(con(), paste0("SELECT itemdescription FROM orders WHERE id = ",orderid,";"))$itemdescription
      })
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
      accounts <- tryCatch({
        accounts <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ac = 'true';"))$wa_number
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        accounts <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ac = 'true';"))$wa_number
      })
      send_template(accounts, body_params, "request_quote_pmt", header, project=sent_wa$project, orderid = orderid, docid = docID)
      tryCatch({
        dbExecute(con(), paste0("UPDATE orders SET status = 'Quote Approved', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),"' WHERE id = ", orderid, ";"))
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        dbExecute(con(), paste0("UPDATE orders SET status = 'Quote Approved', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),"' WHERE id = ", orderid, ";"))
      })
    }
    
    else if (button_details$text == "Proceed Without Payment") {
      sent_wa <- tryCatch({
        sent_wa <- dbGetQuery(con(), paste0("SELECT * FROM sent_wa WHERE id = '",button_details$contextid,"';"))
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        sent_wa <- dbGetQuery(con(), paste0("SELECT * FROM sent_wa WHERE id = '",button_details$contextid,"';"))
      })
      docID <- sent_wa$docid
      orderid <- sent_wa$orderid
      order_description <- tryCatch({
        order_description <- dbGetQuery(con(), paste0("SELECT itemdescription FROM orders WHERE id = ",orderid,";"))$itemdescription
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        order_description <- dbGetQuery(con(), paste0("SELECT itemdescription FROM orders WHERE id = ",orderid,";"))$itemdescription
      })
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
      admin <- tryCatch({
        admin <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ai = 'true';"))$wa_number
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        admin <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ai = 'true';"))$wa_number
      })
      send_template(admin, body_params, "quote_no_pmt", header, project=sent_wa$project, orderid = orderid, docid = docID)
      tryCatch({
        dbExecute(con(), paste0("UPDATE orders SET status = 'Order Go Ahead', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),"' WHERE id = ", orderid, ";"))
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        dbExecute(con(), paste0("UPDATE orders SET status = 'Order Go Ahead', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),"' WHERE id = ", orderid, ";"))
      })
    }
    
    else if (button_details$text == "Complete Project") {
      projectName <- tryCatch({
        projectName <- dbGetQuery(con(), paste0("SELECT project FROM sent_wa WHERE id = '", button_details$contextid, "';"))$project
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        projectName <- dbGetQuery(con(), paste0("SELECT project FROM sent_wa WHERE id = '", button_details$contextid, "';"))$project
      })
      tryCatch({
        dbExecute(con(), paste0("UPDATE projects SET status = 'To be Invoiced', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                                "' WHERE projectname = '", projectName, "';"))
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        dbExecute(con(), paste0("UPDATE projects SET status = 'To be Invoiced', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                                "' WHERE projectname = '", projectName, "';"))
      })
      
      body <- list(
        list("type" = "text", "text" = projectName)
      )
      admin <- tryCatch({
        admin <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ai = 'true';"))$wa_number
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        admin <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ai = 'true';"))$wa_number
      })
      send_template(admin, body, "start_invoice", project = projectName)
      md <- tryCatch({
        md <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE md = 'true';"))$wa_number
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        md <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE md = 'true';"))$wa_number
      })
      send_template(md, body, template_name = 'md_project_completed', project = projectName)
    }
    
    else if (button_details$text == "Approve Invoice") {
      sent_WA <- tryCatch({
        sent_WA <- dbGetQuery(con(), paste0("SELECT * FROM sent_wa WHERE id = '",button_details$contextid,"';"))
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        sent_WA <- dbGetQuery(con(), paste0("SELECT * FROM sent_wa WHERE id = '",button_details$contextid,"';"))
      })
      header <- list(
        'type' = 'document',
        'document' = list(
          'id' = sent_WA$docid,
          'filename' = paste0(sent_WA$invoicename, ".pdf")
        )
      )
      if (button_details$fromid == dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE pm = 'true';"))$wa_number) {
        body_params <- list(list('type' = 'text', 'text' = sent_WA$project))
        main_director <- tryCatch({
          main_director <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE md = 'true';"))$wa_number
        }, 
        error = function(e) {
          con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
          main_director <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE md = 'true';"))$wa_number
        })
        send_template(main_director, body_params, "invoice_approval", heading = header, project = sent_WA$project, docid = sent_WA$docid, invoicename = sent_WA$invoicename)
        tryCatch({
          dbExecute(con(), paste0("UPDATE projects SET status = 'Invoice Sent to Sven', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                                  "' WHERE projectname = '", sent_WA$project, "';"))        }, 
        error = function(e) {
          con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
          dbExecute(con(), paste0("UPDATE projects SET status = 'Invoice Sent to Sven', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                                  "' WHERE projectname = '", sent_WA$project, "';"))        })
      } else {
        invoice_data <- (get_from_api(
          "TaxInvoice", 
          query_params = paste0("$filter=DocumentNumber eq '",sent_WA$invoicename,"'&includeDetail=False&includeCustomerDetails=True&") 
        ))
        admin <- tryCatch({
          admin <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ai = 'true';"))$wa_number
        }, 
        error = function(e) {
          con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
          admin <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ai = 'true';"))$wa_number
        })
        if (invoice_data$TotalResults > 0) {
          invoiceID <- invoice_data$Results$ID
          if (is.null(invoice_data$Results$Customer$Mobile) || invoice_data$Results$Customer$Mobile == "") {
            body_params <- list(
              list('type' = 'text', 'text' = sent_WA$invoicename)
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
              send_template(admin, template_name = "invoice_failed", project = sent_WA$project, body_params = body_params, invoicename = sent_WA$invoicename)
            }
          }
        } else {
          body_params <- list(list('type' = 'text', 'text' = sent_WA$invoicename))
          send_template(admin, body_params = body_params, template_name = "invoice_failed", project = sent_WA$project, body_params = body_params, invoicename = sent_WA$invoicename)
        }
        tryCatch({
          dbExecute(con, paste0("UPDATE projects SET status = 'Invoiced', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                                "' WHERE projectname = '", sent_WA$project, "';"))        }, 
        error = function(e) {
          con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
          dbExecute(con, paste0("UPDATE projects SET status = 'Invoiced', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                                "' WHERE projectname = '", sent_WA$project, "';"))        })
      }
    } else send_template(button_details$from, template_name = "unknown_request")
  } else send_template(button_details$from, template_name = "unknown_request")
}

get_WA_number <- function(number) {
  wa_number <- paste0("27", substr(number, 2, nchar(number)))
  return(wa_number)
}

send_invoice_to_PM <- function(message_details) {
  projectName <- tryCatch({
    projectName <- dbGetQuery(con(), paste0("SELECT project FROM sent_wa WHERE id = '", message_details$contextid, "';"))$project
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    projectName <- dbGetQuery(con(), paste0("SELECT project FROM sent_wa WHERE id = '", message_details$contextid, "';"))$project
  })
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
      pm <- tryCatch({
        pm <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE pm = 'true';"))$wa_number
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        pm <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE pm = 'true';"))$wa_number
      })
      send_template(pm, body_params, "invoice_approval", heading = header, project = projectName, docid = docID, invoicename = message_details$text)
    }} else {
      body_params <- list(
        list('type' = 'text', 'text' = "we were unable to retrieve the invoice details")
      )
      admin <- tryCatch({
        admin <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ai = 'true';"))$wa_number
      }, 
      error = function(e) {
        con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
        admin <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE ai = 'true';"))$wa_number
      })
      send_template(admin, template_name = "invoice_failed", project = projectName, body_params = body_params, invoicename = message_details$text)
    }
  tryCatch({
    dbExecute(con(), paste0("UPDATE projects SET status = 'Invoice Sent to PM', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                            "' WHERE projectname = '", projectName, "';"))  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    dbExecute(con(), paste0("UPDATE projects SET status = 'Invoice Sent to PM', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                            "' WHERE projectname = '", projectName, "';"))  })
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
  active_proj <- tryCatch({
    active_proj <- dbGetQuery(con(), paste0("SELECT projectid FROM projects WHERE status IN ('Not Started', 'In Progress');"))$projectid
  }, 
  error = function(e) {
    con(dbConnect(RPostgres::Postgres(), user = "ucr5l5kv090pne", password = "p54f2fdf2a84201889d0c2eb6e634624192bea1f1a7a1abf423bcb5c7ad2a982c", host = "ec2-54-194-134-97.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "d6hsqvpeb3dbtf"))
    active_proj <- dbGetQuery(con(), paste0("SELECT projectid FROM projects WHERE status IN ('Not Started', 'In Progress');"))$projectid
  })
  projects <- get_projects(get_from_api("TimeTrackingProject","GetActiveProjects")) %>% filter(ID %in% active_proj)
}
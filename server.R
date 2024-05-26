server <- function(input, output, session) {
  
  # Reactive timer to call get_webhooks every 5 seconds
  autoInvalidate <- reactiveTimer(5000)
  # Timer for connection of postgres every 4 mins
  postgresTimer <- reactiveTimer(1000*60*4)
  new_webhooks <- reactiveVal()
  con <- reactiveVal(dbConnect(RPostgres::Postgres(), user = "u2tnmv2ufe7rpk", password = "p899046d336be15351280fd542015420a8e18e22dfe07c1cccaaa8e0e9fb20631", host = "cdgn4ufq38ipd0.cluster-czz5s0kz4scl.eu-west-1.rds.amazonaws.com", port = 5432, dbname = "d6qh1puq26hrth"))
  projects <- reactiveVal(get_query("SELECT * FROM projects;"))
  tasks_sheet <- reactiveVal()
  orders_data <- reactiveVal()
  
  
  # Timer for connection of postgres every 90 mins
  syncProj <- reactiveTimer(1000*60*90)
  
  # observe({
  #   syncProj()
  #   sync_invoices_and_projects()
  #   check_reminders()
  # })
  
  sheet <- reactiveVal()
  tasks <- reactiveVal()
  proj_admin_table <- reactiveVal()
  
  # Project Admin page ----
  output$project_admin_table <- renderDT({
    proj_sheet <- get_query("SELECT * FROM projects")
    if (input$show_only_incomplete_projects) proj_admin_table(proj_sheet %>% filter(status %in% c("Not Started", "In Progress", "Ready for QC", "To be Invoiced")) %>% arrange(desc(projectname)))
    else proj_admin_table(proj_sheet %>% arrange(desc(projectname)))
    datatable(proj_admin_table() %>%
                select(-projectid) %>%
                mutate(images = ifelse(images != "", paste0("<a href='", images,"' target='_blank'>View</a>"), images),
                       active = ifelse(active, "&#10004;", "&#10060;")),
              colnames = c("Project Name", "Customer", "Status", "Added", "Last Update", "Invoice No.", "Images", "Notes", "Active", "Pmt Status"),
              escape = FALSE)
  })
  
  observeEvent(input$update_proj_status, {
    if (length(input$project_admin_table_rows_selected)==0) show_alert("No project was selected.")
    else {
      if (length(input$project_admin_table_rows_selected)==1) choices <- c("Not Started", "In Progress", "Ready for QC", "To be Invoiced", "Invoiced","No Charge","Cancelled")
      else choices <- c("Not Started", "In Progress", "Ready for QC", "To be Invoiced","No Charge","Cancelled")
      modalDialog(
        title = "Update the Status of Selected Projects",
        div(
          style = "display:inline-block",
          radioGroupButtons(
            "proj_status_update",
            "Select status:",
            choices = choices,
            width = "270px"
          )
        ),
        div(
          id = "update_status_invoice_no",
          style = "display:inline-block",
          textInput(
            "proj_status_update_inv",
            "Enter Invoice Number:",
            value = "INV22",
            width = "270px"
          )
        ),
        size = "s",
        easyClose = FALSE,
        footer = div(
          class = "pull-right container",
          actionButton(
            inputId = "dismiss_modal",
            label = "Close",
            icon = icon("xmark"),
            class = "add_proj"
          ),
          actionButton(
            inputId = "confirm_proj_status_update",
            label = "Update Status",
            icon = icon("check"),
            class = "back"
          )
        )
      ) %>% showModal()
    }
    
    
    observeEvent(input$proj_status_update, {
      if (input$proj_status_update == "Invoiced") shinyjs::show("update_status_invoice_no")
      else shinyjs::hide("update_status_invoice_no")
    })
  })
  
  observeEvent(input$confirm_proj_status_update,{
    projects <- proj_admin_table()[input$project_admin_table_rows_selected,]
    projectids <- paste("'", projects$projectid, "'", collapse = ", ")
    if (input$proj_status_update == "Invoiced") {
      execute(paste0("UPDATE projects SET status = '", input$proj_status_update, "', invoiceno = '",input$proj_status_update_inv,"', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                     "' WHERE projectid IN (", projectids, ");"))
    }
    else {
      execute(paste0("UPDATE projects SET status = '", input$proj_status_update, "', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),
                     "' WHERE projectid IN (", projectids, ");"))
      
      if (input$proj_status_update == "Ready for QC") {
        for (i in 1:nrow(projects)) {
          body <- list(
            list("type" = "text", "text" = projects$projectname[i])
          )
          QC <- get_pm_wa()
          send_template(QC, body, "all_tasks_completed", project = projects$projectname[i])
        }
      }
      if (input$proj_status_update == "To be Invoiced") {
        for (i in 1:nrow(projects)) {
          body <- list(
            list("type" = "text", "text" = projects$projectname[i])
          )
          ac <- get_ac_wa()
          admin <- get_ai_wa()
          send_template(admin, body, "start_invoice", project = projects$projectname[i])
          send_template(ac, body, "md_project_completed", project = projects$projectname[i])
          md <- get_md_wa()
          send_template(md, body, template_name = 'md_project_completed', project = projects$projectname[i])
        }
      }
    }
    proj_sheet <- get_query("SELECT * FROM projects")
    if (input$show_only_incomplete_projects) proj_admin_table(proj_sheet %>% filter(status %in% c("Not Started", "In Progress", "Ready for QC", "To be Invoiced")) %>% arrange(desc(projectname)))
    else proj_admin_table(proj_sheet %>% arrange(desc(projectname)))
    removeModal()
  })
  
  observeEvent(input$refreshProjAdmin, {
    dbDisconnect(isolate(con())) 
    con(dbConnect(RPostgres::Postgres(), user = "u2tnmv2ufe7rpk", password = "p899046d336be15351280fd542015420a8e18e22dfe07c1cccaaa8e0e9fb20631", host = "cdgn4ufq38ipd0.cluster-czz5s0kz4scl.eu-west-1.rds.amazonaws.com", port = 5432, dbname = "d6qh1puq26hrth"))
  })
  
  # Orders page ----
  observeEvent(input$refreshOrders, {
    if (input$include_received) {
      orders_data(get_query("SELECT * FROM orders;") %>% arrange(desc(id)))
    }
    else {
      orders_data(get_query("SELECT * FROM orders WHERE status != 'Arrived';") %>% arrange(desc(id)))
    }
  })
  
  output$order_table <- renderDT({
    if (input$include_received) {
      orders_data(get_query("SELECT * FROM orders;") %>% arrange(desc(id)))
    }
    else {
      orders_data(get_query("SELECT * FROM orders WHERE status != 'Arrived';") %>% arrange(desc(id)))
    }
    datatable(orders_data() %>%
                mutate(
                  "ID" = id,
                  "Project" = project,
                  "By" = submittedby,
                  "Date" = datesubmitted,
                  "Description" = itemdescription,
                  "Status" = status,
                  "Last Update" = lastupdate,
                  "Courier" = courier,
                  "ETA" = eta,
                  "Quotes" = ifelse(quotes != "", paste0("<a href='", quotes,"' target='_blank'>View</a>"), quotes),
                  "Images" = ifelse(images != "", paste0("<a href='", images,"' target='_blank'>View</a>"), images)
                ) %>%
                select(ID, Project, By, Date, Description, Status, "Last Update", Courier, ETA, Quotes, Images),
              escape = FALSE, selection = list(mode = 'single', target = 'cell'))
  })
  
  orders_selected_col <- reactiveVal()
  orders_selected_row <- reactiveVal()
  
  observeEvent(input$order_table_cells_selected,{
    
    if (nrow(input$order_table_cells_selected) > 0) {
      selected_col_index <- input$order_table_cells_selected[,2]
      selected_col <- switch(selected_col_index,
                             "id",
                             "project",
                             "submittedby",
                             "datesubmitted",
                             "itemdescription",
                             "status",
                             "lastupdate",
                             "courier",
                             "eta",
                             "quotes",
                             "images")
      orders_selected_col(selected_col)
      orders_selected_row(orders_data()[input$order_table_cells_selected[,1],])
      if (selected_col == "quotes") {
        quotes <- get_query(paste0("SELECT quotes FROM orders WHERE id = ",orders_selected_row()$id,";"))$quotes
        if (is.na(unique(quotes))) {
          modalDialog(
            title = "Upload a Quote",
            div(
              style = "display:inline-block",
              checkboxInput(
                "approve_without_quotes",
                "Submit for approval without quotes",
                value = FALSE,
                width = "270px"
              )
            ),
            div(
              id = "file_div",
              style = "display:inline-block",
              fileInput(
                "quote_file",
                "Upload:",
                multiple = TRUE,
                width = "270px"
              )
            ),
            div(
              id = "order_amount_div",
              style = "display:inline-block",
              numericInput(
                "order_amount",
                "Amount Needed in Rands:",
                value = 0,
                width = "270px"
              )
            ),
            size = "s",
            easyClose = FALSE,
            footer = div(
              class = "pull-right container",
              fluidRow(textOutput("file_upload_error")),
              actionButton(
                inputId = "dismiss_modal",
                label = "Close",
                icon = icon("xmark"),
                class = "add_proj"
              ),
              actionButton(
                inputId = "upload_files",
                label = "Confirm",
                icon = icon("plus"),
                class = "back"
              )
            )
          ) %>% showModal()
        }
        
        observeEvent(input$approve_without_quotes,{
          if (input$approve_without_quotes) {
            shinyjs::hide("file_div")
            shinyjs::show("order_amount_div")
          }
          else {
            shinyjs::show("file_div")
            shinyjs::hide("order_amount_div")
          }
        })
        
      } else if (selected_col %in% c("status", "courier", "eta")) {
        if (orders_selected_row()$status %in% c("Paid", "Order Go Ahead")) {
          modalDialog(
            title = "Update Order",
            div(
              style = "display:inline-block",
              selectizeInput(
                "order_courier",
                "Courier Status:",
                choices = c("Arranged", "To be arranged", "Not necessary"),
                width = "270px"
              )
            ),
            div(
              stype = "display:inline-block",
              daterangepicker(
                "order_eta",
                "Expected Arrival Date:",
                Sys.Date(),
                style = "border:1px solid lightgrey; height:32px; width:270px; text-align: center;",
                options = daterangepickerOptions(singleDatePicker = TRUE)
              )
            ),
            size = "s",
            easyClose = FALSE,
            footer = div(
              class = "pull-right container",
              actionButton(
                inputId = "dismiss_modal",
                label = "Close",
                icon = icon("xmark"),
                class = "add_proj"
              ),
              actionButton(
                inputId = "update_order_to_ordered",
                label = "Update Order",
                icon = icon("pen"),
                class = "back"
              )
            )
          ) %>% showModal()
        } else if (orders_selected_row()$status %in% c("Ordered",'Purchase Go Ahead')) {
          modalDialog(
            title = "Complete Order",
            size = "s",
            easyClose = FALSE,
            footer = div(
              class = "pull-right container",
              actionButton(
                inputId = "dismiss_modal",
                label = "Close",
                icon = icon("xmark"),
                class = "add_proj"
              ),
              actionButton(
                inputId = "complete_order",
                label = "Order Arrived",
                icon = icon("check"),
                class = "back"
              )
            )
          ) %>% showModal()  
        }
      }
    }
  })
  
  observeEvent(input$complete_order, {
    execute(paste0("UPDATE orders SET status = 'Arrived', lastupdate = '",
                              format(Sys.Date(), format = "%d-%m-%Y"),
                              "' WHERE id = ", orders_selected_row()$id, ";"))
    wa_id <- get_query(paste0("SELECT wa_number FROM active_ts WHERE name = '",orders_selected_row()$submittedby,"';"))$wa_number
    body_params <- list(
      list('type' = 'text', 'text' = orders_selected_row()$id),
      list('type' = 'text', 'text' = orders_selected_row()$itemdescription)
    )
    send_template(wa_id, body_params = body_params, template_name = 'order_arrived', project = orders_selected_row()$project, orderid = orders_selected_row()$id)
    pm <- get_pm_wa()
    if (pm != wa_id) send_template(pm, body_params = body_params, template_name = 'order_arrived', project = orders_selected_row()$project, orderid = orders_selected_row()$id)
    
    if (input$include_received) {
      orders_data(get_query("SELECT * FROM orders;") %>% arrange(desc(id)))
    }
    else {
      orders_data(get_query("SELECT * FROM orders WHERE status != 'Arrived';") %>% arrange(desc(id)))
    }
    removeModal()
  })
  
  observeEvent(input$update_order_to_ordered,{
    execute(paste0("UPDATE sent_wa SET responded = 'true' WHERE message = 'quote_no_pmt' AND orderid = '", orders_selected_row()$id, "';"))
    
    execute(paste0("UPDATE orders SET status = 'Ordered', lastupdate = '",
                              format(Sys.Date(), format = "%d-%m-%Y"), "', courier = '",
                              input$order_courier, "', eta = '", format(input$order_eta[1], format = "%d-%m-%Y"),
                              "' WHERE id = ", orders_selected_row()$id, ";"))
    wa_id <- get_query(paste0("SELECT wa_number FROM active_ts WHERE name = '",orders_selected_row()$submittedby,"';"))$wa_number
    body_params <- list(
      list('type' = 'text', 'text' = orders_selected_row()$id),
      list('type' = 'text', 'text' = orders_selected_row()$itemdescription),
      list('type' = 'text', 'text' = format(input$order_eta[1], format = "%d %b"))
    )
    send_template(wa_id, body_params = body_params, template_name = 'ordered', project = orders_selected_row()$project, orderid = orders_selected_row()$id)
    pm <- get_pm_wa()
    if (pm != wa_id) send_template(pm, body_params = body_params, template_name = 'ordered', project = orders_selected_row()$project, orderid = orders_selected_row()$id)
    if (input$include_received) {
      orders_data(get_query("SELECT * FROM orders;") %>% arrange(desc(id)))
    }
    else {
      orders_data(get_query("SELECT * FROM orders WHERE status != 'Arrived';") %>% arrange(desc(id)))
    }
    removeModal()
  })
  
  observeEvent(input$upload_files, {
    output$file_upload_error <- renderText({"Processing, please wait..."})
    if (input$approve_without_quotes) {
      if (input$order_amount == 0) {
        output$file_upload_error <- renderText({"Please enter an amount."})
      } else {
        execute(paste0("UPDATE sent_wa SET responded = 'true' WHERE message = 'new_order' AND orderid = '", orders_selected_row()$id, "';"))
        ac <- get_ac_wa()
        body <- list(
          list("type" = "text", "text" = orders_selected_row()$project),
          list("type" = "text", "text" = orders_selected_row()$id),
          list("type" = "text", "text" = orders_selected_row()$itemdescription),
          list("type" = "text", "text" = as.character(input$order_amount))
        )
        #KARLA Test this
        send_template(ac, body_params = body, template_name = 'request_payment_without_quote', project = orders_selected_row()$project, orderid = orders_selected_row()$id)
        execute(paste0("UPDATE orders SET status = 'Payment Requested', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),"' WHERE id = ",orders_selected_row()$id,";" ))
      }
    }
    else {
      if (nrow(input$quote_file) == 0) output$file_upload_error <- renderText({"No files were selected."})
      else {        
        execute(paste0("UPDATE sent_wa SET responded = 'true' WHERE message = 'new_order' AND orderid = '", orders_selected_row()$id, "';"))

        drive_auth(path = google_drive_service_acc)
        path <- paste0("ProjectQuotes/",orders_selected_row()$id,"/")
        try(path <- drive_mkdir(as.character(orders_selected_row()$id), path = "ProjectQuotes/", overwrite = FALSE))
        for (i in 1:nrow(input$quote_file)) {
          row <- input$quote_file[i, ]
          drive_upload(row$datapath, 
                       name = row$name, 
                       type = row$type, 
                       path = path)
          body = list(
            'messaging_product' = 'whatsapp',
            'file' = upload_file(row$datapath),
            'name' = row$name
          )
          res <- VERB("POST", url = "https://graph.facebook.com/v18.0/256491817549838/media", body = body, add_headers("Authorization" = paste0("Bearer ",wa_token)), encode = 'multipart')
          docID <- fromJSON(content(res, 'text'))$id
          header <- list(
            'type' = 'document',
            'document' = list(
              'id' = docID,
              'filename' = row$name
            )
          )
          body_params <- list(
            list('type' = 'text', 'text' = as.character(i)),
            list('type' = 'text', 'text' = as.character(nrow(input$quote_file))),
            list('type' = 'text', 'text' = as.character(orders_selected_row()$id)),
            list('type' = 'text', 'text' = orders_selected_row()$itemdescription)
          )
          md <- get_md_wa()
          message <- send_template(md, body_params, "quote_approval", header, project=orders_selected_row()$project, orderid = orders_selected_row()$id, docid = docID)
          if (!message) output$file_upload_error <- renderText({"Unable to send the quotes for approval."})
        }
        execute(paste0("UPDATE orders SET status = 'Quotes Uploaded', lastupdate = '",format(Sys.Date(), format = "%d-%m-%Y"),"' WHERE id = ",orders_selected_row()$id,";" ))
        
        folder <- drive_find(pattern = as.character(orders_selected_row()$id), type = "folder")
        execute(paste0("UPDATE orders SET quotes = '",folder$drive_resource[[1]]$webViewLink, "' WHERE id = ",orders_selected_row()$id,";" ))
      }
    }
    if (input$include_received) {
      orders_data(get_query("SELECT * FROM orders;") %>% arrange(desc(id)))
    }
    else {
      orders_data(get_query("SELECT * FROM orders WHERE status != 'Arrived';") %>% arrange(desc(id)))
    }
    output$file_upload_error <- renderText({""})
    removeModal()
  })
  
  observeEvent(input$addOrder, {
    active_employees <- get_query("SELECT name FROM active_ts")$name
    proj_choices <- get_query("SELECT projectname FROM projects WHERE status IN ('Not Started', 'In Progress');")$projectname
    modalDialog(
      title = "Submit New Order",
      div(
        style = "display:inline-block",
        textInput(
          inputId = "new_order_description",
          label = "Order Description:",
          width = "270px"
        )
      ),
      div(
        style = "display:inline-block",
        selectInput(
          inputId = "new_order_empl",
          label = "Submitted By:",
          choices = unique(c("Lisa-Marie", "Sven", "Deneys", "Ursula", active_employees)),
          width = "270px"
        )
      ),
      div(
        style = "display:inline-block",
        selectInput(
          inputId = "new_order_proj",
          label = "Project:",
          choices = proj_choices,
          width = "270px"
        )
      ),
      size = "s",
      easyClose = FALSE,
      footer = div(
        class = "pull-right container",
        fluidRow(textOutput("add_order_error")),
        actionButton(
          inputId = "dismiss_modal",
          label = "Close",
          icon = icon("xmark"),
          class = "add_proj"
        ),
        actionButton(
          inputId = "add_order",
          label = "Add Order",
          icon = icon("plus"),
          class = "back"
        )
      )
    ) %>% showModal()  
  })
  
  observeEvent(input$add_order, {
    from <- get_query(paste0("SELECT wa_number FROM active_ts WHERE name = '",input$new_order_empl,"';"))$wa_number
    df <- data.frame(
      "text" = paste0("O", substr(input$new_order_proj, 1,4), " ", input$new_order_description),
      "from" = from
    )
    create_order(df)
    if (input$include_received) {
      orders_data(get_query("SELECT * FROM orders;") %>% arrange(desc(id)))
    }
    else{
      orders_data(get_query("SELECT * FROM orders WHERE status != 'Arrived';") %>% arrange(desc(id)))
    } 
    removeModal()
  })
  
  observeEvent(input$dismiss_modal, {
    removeModal()
  })
  tasks <- reactiveVal(get_tasks(get_from_api("TimeTrackingTask","Get","$filter=Active eq true&includeProjectTasks=True&")))
  
  
  # Project Management -----
  observeEvent(input$sidebartabs,{
    
    if (input$sidebartabs == "project_planner") {
      tasks_sheet(unique(get_query("SELECT * FROM tasks")))
      proj_timevis_data <- reactiveVal(data.frame())
      filtered_projects <- projects()
      
      choices_display <- paste0(filtered_projects$projectname, " (", filtered_projects$customer, ")")
      named_vector <- setNames(filtered_projects$projectname, choices_display) %>%
        sort(decreasing = TRUE)
      updateSelectizeInput(session, "proj_plan", label = NULL, choices = c("Select Project", named_vector))
      
      observe({
        if (nrow(tasks_sheet() > 0) && !is.null(input$proj_plan)) {
          df <-  tasks_sheet() %>% filter(projectname == input$proj_plan)
          if (!is.null(input$proj_plan) && nrow(df > 0)) {
            proj_timevis_data(updateProjTimevis(df))
          } else {
            proj_timevis_data(data.frame())
          }
        }
      })
      
      output$projectTimeVis <- renderTimevis({
        timevis(data = proj_timevis_data())
      })
      
      observeEvent(input$addTask, {
        enabled_users <- get_query("SELECT name FROM active_ts")$name
        proj_id <- (projects() %>% filter(projectname == input$proj_plan))$projectid
        tasks <- filter(tasks(), grepl(paste0("\\b", proj_id, "\\b"), ProjectID))$Name
        removeModal()
        shiny::modalDialog(
          title = "Add Task to Project",
          column(width = 8,
                 div(
                   style = "display: inline-block;",
                   daterangepicker(
                     "task_dates",
                     "Planned Dates: ",
                     start = Sys.Date(),
                     end = Sys.Date(),
                     style = "border:1px solid lightgrey; height:32px; width:270px; text-align: center;"
                   )
                 ),
                 div(
                   style = "display: inline-block;",
                   airDatepickerInput(
                     "task_start_time",
                     "Planned Start Time: ",
                     timepicker = TRUE,
                     timepickerOpts = timepickerOptions(
                       minHours = 6,
                       maxHours = 18,
                       minutesStep = 10
                     ),
                     onlyTimepicker = TRUE,
                     inline = TRUE,
                     value = paste(Sys.Date(), "07:00"),
                     width = "270px"
                   )
                 ),
                 div(
                   style = "display: inline-block;",
                   airDatepickerInput(
                     "task_end_time",
                     "Planned End Time: ",
                     timepicker = TRUE,
                     timepickerOpts = timepickerOptions(
                       minHours = 6,
                       maxHours = 18,
                       minutesStep = 10
                     ),
                     onlyTimepicker = TRUE,
                     inline = TRUE,
                     value = paste(Sys.Date(), "16:00"),
                     width = "270px"
                   )
                 ),
                 div(
                   style = "display: inline-block;",
                   shiny::selectizeInput(
                     inputId = "task_name",
                     label = "Task:",
                     choices = tasks,
                     width = "270px"
                   )
                 ),
                 div(
                   style = "display: inline-block;",
                   shiny::textInput(
                     inputId = "task_description",
                     label = "Description:",
                     value = "",
                     width = "270px"
                   )
                 ),
                 div(
                   style = "display: inline-block;",
                   selectizeInput(
                     "new_task_empl",
                     "Alocate Employee:",
                     choices = c("None", enabled_users),
                     width = "270px"
                   )
                 )
          ),
          size = "s",
          easyClose = FALSE,
          footer = div(
            class = "pull-right container",
            shiny::actionButton(
              inputId = "addTaskProj",
              label = "Add Task",
              icon = shiny::icon("plus"),
              class = "btn-info"
            ),
            shiny::actionButton(
              inputId = "dismiss_modal",
              label = "Close",
              class = "btn-danger"
            )
          )
        ) %>% shiny::showModal() 
      })
      
      observeEvent(input$proj_plan, {
        proj_id <- (projects() %>% filter(projectname == input$proj_plan))$projectid
        current_proj <- input$proj_plan
        if (!is.null(current_proj) && current_proj != "" && length(proj_id) != 0) {
          shinyjs::show("projectTasks_div")
          shinyjs::show("addTask_div")
          df <- tasks_sheet() %>% filter(projectname == input$proj_plan)
          if (nrow(df) > 0) shinyjs::show("projectTable_div")
          else shinyjs::hide("projectTable_div")
        } else {
          shinyjs::hide("projectTable_div")
          shinyjs::hide("projectTasks_div")
          shinyjs::hide("addTask_div")
        }
      })
      
      observeEvent(input$addTaskProj, {
        latest_id <- max(get_query("SELECT taskid FROM tasks")$taskid)
        latest_task <- tasks_sheet()[tasks_sheet()$taskid == latest_id, -which(names(tasks_sheet()) == 'taskid')]
        new_item <- data.frame(
          projectname = input$proj_plan,
          taskname = input$task_name,
          description = input$task_description,
          status = "Not Started",
          employee = input$new_task_empl,
          plannedstart = paste(format(input$task_dates[1], format = "%Y-%m-%d"), format(input$task_start_time, format = "%H:%M:%S")),
          plannedcompletion = paste(format(input$task_dates[2], format = "%Y-%m-%d"), format(input$task_end_time, format = "%H:%M:%S")),
          colour = task_colors[input$task_name]
        )
        rownames(new_item) <- NULL
        rownames(latest_task) <- NULL
        if (!identical(latest_task, new_item)) {
          print(paste0("Add Task: ", nrow(new_item), ": ", paste(new_item, collapse = " | ")))
          execute(paste0("INSERT INTO tasks (taskid, projectname, taskname, description, status, employee, plannedstart, plannedcompletion, colour) VALUES (",latest_id+1,",'",new_item$projectname,
                                    "','", new_item$taskname, "', '", new_item$description,"', '", new_item$status, "', '", 
                                    new_item$employee,  "', '", new_item$plannedstart, "', '", 
                                    new_item$plannedcompletion, "', '", new_item$colour, "');"))
          tasks_sheet(unique(get_query("SELECT * FROM tasks")))
          proj_timevis_data(updateProjTimevis(tasks_sheet() %>% filter(projectname == input$proj_plan))) 
        }
        shiny::removeModal()
      })
      
      observeEvent(input$projectTimeVis_selected, {
        selected_task <- get_query(paste0("SELECT * FROM tasks WHERE taskid = ", input$projectTimeVis_selected, ";"))
        enabled_users <- get_query(paste0("SELECT name FROM active_ts;"))$name
        proj_id <- (isolate(projects()) %>% filter(projectname == input$proj_plan))$projectid
        tasks <- filter(isolate(tasks()), grepl(paste0("\\b", proj_id, "\\b"), ProjectID))$Name
        shiny::modalDialog(
          title = "Edit Task",
          column(
            width = 6,
            div(
              style = "display: inline-block;",
              shiny::selectizeInput(
                inputId = "edit_task_name",
                label = "Task:",
                choices = tasks,
                selected = selected_task$taskname,
                width = "270px"
              )
            ),
            div(
              style = "display: inline-block;",
              shiny::textInput(
                inputId = "edit_task_description",
                label = "Description:",
                value = selected_task$description,
                width = "270px"
              )
            ),
            div(
              style = "display: inline-block;",
              selectizeInput(
                "edit_task_empl",
                "Alocate Employee:",
                choices = c("None", enabled_users),
                selected = selected_task$employee,
                width = "270px"
              )
            )
          ),
          column(
            width = 6,
            div(
              style = "display: inline-block;",
              daterangepicker(
                "edit_task_dates",
                "Planned Dates: ",
                start = as.POSIXct(selected_task$plannedstart),
                end = as.POSIXct(selected_task$plannedcompletion),
                style = "border:1px solid lightgrey; height:32px; width:270px; text-align: center;"
              )
            ),
            div(
              style = "display: inline-block;",
              airDatepickerInput(
                "edit_task_start_time",
                "Planned Start Time: ",
                timepicker = TRUE,
                timepickerOpts = timepickerOptions(
                  minHours = 6,
                  maxHours = 18,
                  minutesStep = 10
                ),
                onlyTimepicker = TRUE,
                inline = TRUE,
                value = as.POSIXct(selected_task$plannedstart),
                width = "270px"
              )
            ),
            div(
              style = "display: inline-block;",
              airDatepickerInput(
                "edit_task_end_time",
                "Planned End Time: ",
                timepicker = TRUE,
                timepickerOpts = timepickerOptions(
                  minHours = 6,
                  maxHours = 18,
                  minutesStep = 10
                ),
                onlyTimepicker = TRUE,
                inline = TRUE,
                value = as.POSIXct(selected_task$plannedcompletion),
                width = "270px"
              )
            )
          ),
          div(
            style = "display: inline-block;",
            radioGroupButtons(
              "edit_task_status",
              "Status:",
              choiceValues = c("Not Started", "In Progress", "Completed", "QC Passed"), 
              choiceNames = c("Not Started", "In Progress", "Completed", "QC Passed"),
              selected = selected_task$status,
              justified = TRUE, 
              width = "540px"
            ),
            tags$style(HTML("
                    .btn-group input[type='radio'][value='Not Started'] { background-color: red; }
                    .btn-group input[type='radio'][value='In Progress'] { background-color: blue; }
                    .btn-group input[type='radio'][value='Completed'] { background-color: green; }
                    .btn-group input[type='radio'][value='QC Passed'] { background-color: purple; }
                  "))
          ),
          size = "m",
          easyClose = FALSE,
          footer = div(
            class = "pull-right container",
            shiny::actionButton(
              inputId = "editTaskProj",
              label = "Apply",
              icon = shiny::icon("pencil"),
              class = "btn-primary"
            ),
            shiny::actionButton(
              inputId = "dltTaskProj",
              label = "Delete",
              icon = shiny::icon("trash"),
              class = "btn-danger"
            ),
            shiny::actionButton(
              inputId = "dismiss_modal",
              label = "Cancel",
              icon = shiny::icon("xmark"),
              class = "btn-default"
            )
          )
        ) %>% shiny::showModal() 
      })
      
      observeEvent(input$editTaskProj, {
        execute(paste0("UPDATE tasks SET taskname = '",input$edit_task_name,"', description = '",input$edit_task_description,
                                  "', status = '",input$edit_task_status,"', employee = '",input$edit_task_empl,
                                  "', plannedstart = '",paste(format(input$edit_task_dates[1], format = "%Y-%m-%d"), format(input$edit_task_start_time, format = "%H:%M:%S")),
                                  "', plannedcompletion = '",paste(format(input$edit_task_dates[2], format = "%Y-%m-%d"), format(input$edit_task_end_time, format = "%H:%M:%S")),
                                  "', colour = '",task_colors[input$edit_task_name],"' WHERE taskid = ", input$projectTimeVis_selected,";"))
        if (input$edit_task_status == "In Progress")  {
          execute(paste0("UPDATE projects SET status = 'In Progress' WHERE projectname = '", input$proj_plan,"';"))
        }
        if (input$edit_task_status == "QC Passed") {
          df <- data.frame(
            "task" = input$projectTimeVis_selected,
            "project" = input$proj_plan
          )
          task_QC_passed(df)
        }
        if (input$edit_task_status == "Completed") tasks_ready_for_QC(input$projectTimeVis_selected)
        proj_timevis_data(updateProjTimevis(get_query(paste0("SELECT * FROM tasks WHERE projectname = '",input$proj_plan, "';")))) 
        removeModal()
      })
      
      observeEvent(input$dltTaskProj, {
        execute(paste0("DELETE FROM tasks WHERE taskid = ", input$projectTimeVis_selected,";"))
        proj_timevis_data(get_query(paste0("SELECT * FROM tasks WHERE projectname = '",input$proj_plan, "';")))
        removeModal()
      })
      
      observeEvent(input$dismiss_modal, {
        removeModal()
      })
      
    }
  })
  
  # Timeline Overview page ----
  observeEvent(input$sidebartabs,{
    if (input$sidebartabs == "timeline_overview") {
      tasks_sheet(unique(get_query("SELECT * FROM tasks")))
      updateSelectizeInput(session, "tl_ov_employees", choices = c("All", unique(tasks_sheet()$employee)))
      updateSelectizeInput(session, "tl_ov_project", choices = c("All", unique(tasks_sheet()$projectname)))
      updateSelectizeInput(session, "tl_ov_task", choices = c("All", unique(tasks_sheet()$taskname)))
    }
  })
  
  output$overviewTimeVis <- renderTimevis({
    df <- tasks_sheet()
    
    if (input$tl_ov_employees != "All") df <- df %>% filter(employee == input$tl_ov_employees)
    if (input$tl_ov_project != "All") df <- df %>% filter(projectname == input$tl_ov_project)
    if (input$tl_ov_task != "All") df <- df %>% filter(taskname == input$tl_ov_task)
    groups <- data.frame(
      id = unique(df$employee),
      content = unique(df$employee)
    )
    if (nrow(df)>0) {
      timevis_data <- data.frame(
        content = templateTask(df$taskname, df$description,df$projectname,df$status),
        id = df$taskid,
        start = as.POSIXct(df$plannedstart),
        end = as.POSIXct(df$plannedcompletion),
        group = df$employee,
        style = paste("background-color:", df$colour, "; font-size:5px")
      )
    } else timevis_data <- data.frame()
    
    timevis(data = timevis_data, groups = groups) %>%
      setWindow(paste(input$tl_ov_dates[1],"07:00") , paste(input$tl_ov_dates[2],"16:00"))
  })
  
  observeEvent(input$overviewTimeVis_selected, {
    selected_task <- get_query(paste0("SELECT * FROM tasks WHERE taskid = ", input$overviewTimeVis_selected, ";"))
    enabled_users <- get_query(paste0("SELECT name FROM active_ts;"))$name
    proj_id <- (isolate(projects()) %>% filter(customername == selected_task$projectname))$projectid
    tasks <- filter(isolate(tasks()), grepl(paste0("\\b", proj_id, "\\b"), ProjectID))$Name
    shiny::modalDialog(
      title = "Edit Task",
      column(
        width = 6,
        div(
          style = "display: inline-block;",
          shiny::selectizeInput(
            inputId = "edit_task_name_o",
            label = "Task:",
            choices = tasks,
            selected = selected_task$taskname,
            width = "270px"
          )
        ),
        div(
          style = "display: inline-block;",
          shiny::textInput(
            inputId = "edit_task_description_o",
            label = "Description:",
            value = selected_task$description,
            width = "270px"
          )
        ),
        div(
          style = "display: inline-block;",
          selectizeInput(
            "edit_task_empl_o",
            "Alocate Employee:",
            choices = c("None", enabled_users),
            selected = selected_task$employee,
            width = "270px"
          )
        )
      ),
      column(
        width = 6,
        div(
          style = "display: inline-block;",
          daterangepicker(
            "edit_task_dates_o",
            "Planned Dates: ",
            start = as.POSIXct(selected_task$plannedstart),
            end = as.POSIXct(selected_task$plannedcompletion),
            style = "border:1px solid lightgrey; height:32px; width:270px; text-align: center;"
          )
        ),
        div(
          style = "display: inline-block;",
          airDatepickerInput(
            "edit_task_start_time_o",
            "Planned Start Time: ",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(
              minHours = 6,
              maxHours = 18,
              minutesStep = 10
            ),
            onlyTimepicker = TRUE,
            inline = TRUE,
            value = as.POSIXct(selected_task$plannedstart),
            width = "270px"
          )
        ),
        div(
          style = "display: inline-block;",
          airDatepickerInput(
            "edit_task_end_time_o",
            "Planned End Time: ",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(
              minHours = 6,
              maxHours = 18,
              minutesStep = 10
            ),
            onlyTimepicker = TRUE,
            inline = TRUE,
            value = as.POSIXct(selected_task$plannedcompletion),
            width = "270px"
          )
        )
      ),
      div(
        style = "display: inline-block;",
        radioGroupButtons(
          "edit_task_status_o",
          "Status:",
          choiceValues = c("Not Started", "In Progress", "Completed", "QC Passed"), 
          choiceNames = c("Not Started", "In Progress", "Completed", "QC Passed"),
          selected = selected_task$status,
          justified = TRUE, 
          width = "540px"
        ),
        tags$style(HTML("
                    .btn-group input[type='radio'][value='Not Started'] { background-color: red; }
                    .btn-group input[type='radio'][value='In Progress'] { background-color: blue; }
                    .btn-group input[type='radio'][value='Completed'] { background-color: green; }
                    .btn-group input[type='radio'][value='QC Passed'] { background-color: purple; }
                  "))
      ),
      size = "m",
      easyClose = FALSE,
      footer = div(
        class = "pull-right container",
        shiny::actionButton(
          inputId = "editTaskProj_o",
          label = "Apply",
          icon = shiny::icon("pencil"),
          class = "btn-primary"
        ),
        shiny::actionButton(
          inputId = "dltTaskProj_o",
          label = "Delete",
          icon = shiny::icon("trash"),
          class = "btn-danger"
        ),
        shiny::actionButton(
          inputId = "dismiss_modal",
          label = "Cancel",
          icon = shiny::icon("xmark"),
          class = "btn-default"
        )
      )
    ) %>% shiny::showModal() 
  })
  
  observeEvent(input$editTaskProj_o, {
    execute(paste0("UPDATE tasks SET taskname = '",input$edit_task_name_o,"', description = '",input$edit_task_description_o,
                              "', status = '",input$edit_task_status_o,"', employee = '",input$edit_task_empl_o,
                              "', plannedstart = '",paste(format(input$edit_task_dates_o[1], format = "%Y-%m-%d"), format(input$edit_task_start_time_o, format = "%H:%M:%S")),
                              "', plannedcompletion = '",paste(format(input$edit_task_dates_o[2], format = "%Y-%m-%d"), format(input$edit_task_end_time_o, format = "%H:%M:%S")),
                              "', colour = '",task_colors[input$edit_task_name_o],"' WHERE taskid = ", input$overviewTimeVis_selected,";"))
    if (input$edit_task_status_o == "In Progress")  {
      execute(paste0("UPDATE projects SET status = 'In Progress' WHERE projectname = '", selected_task$projectname,"';"))
    }
    if (input$edit_task_status == "QC Passed") {
      df <- data.frame(
        "task" = input$overviewTimeVis_selected,
        "project" = selected_task$projectname
      )
      task_QC_passed(df)
    }
    if (input$edit_task_status == "Completed") tasks_ready_for_QC(input$overviewTimeVis_selected)
    proj_timevis_data(updateProjTimevis(get_query(paste0("SELECT * FROM tasks WHERE projectname = '",selected_task$projectname, "';")))) 
    removeModal()
  })
  
  observeEvent(input$dltTaskProj, {
    execute(paste0("DELETE FROM tasks WHERE taskid = ", overviewTimeVis_selected,";"))
    proj_timevis_data(get_query(paste0("SELECT * FROM tasks WHERE projectname = '",selected_task$projectname, "';")))
    removeModal()
  })
  
  observeEvent(input$dismiss_modal, {
    removeModal()
  })
  
  # User management page - postgres ----
  observeEvent(input$sidebartabs,{
    
    if (input$sidebartabs == "user_management") {
      sage <- get_users(get_from_api("TimeTrackingUser"))
      enabled_users <- reactiveVal(get_query("SELECT * FROM active_ts"))
      
      output$active_empl_table <- renderDT({
        df <- enabled_users() %>%
          select(clock, pm, md, ac, ap, ai) %>%
          rename(
            "Clocking Employee" = clock, 
            "Project Manager" = pm, 
            "Main Director" = md, 
            "Accounts" = ac,
            "Admin - Purchases" = ap,
            "Admin - Invoicing" = ai
          ) %>%
          rowwise() %>%
          mutate(Role = paste(names(select(., "Clocking Employee", "Project Manager", "Main Director", "Accounts", "Admin - Purchases", "Admin - Invoicing"))[unlist(c_across())], collapse = "<br>")) %>%
          select(Role)
        datatable(
          enabled_users() %>%
            select(name, wa_number) %>%
            rename(
              "Name" = name, 
              "WhatsApp" = wa_number
            ) %>%
            bind_cols(df),
        editable = list(target = 'cell', disable = list(columns = c(1, 3))),
        escape = FALSE
      )
      })
      
      observeEvent(input$addEmployee, {
        users <- (sage %>% filter(!(FirstName %in% enabled_users()$name)))$FirstName
        showModal(modalDialog(
          title = "Add User",
          size = "s",
          selectInput("new_user_name", "Select User:", choices = users),
          checkboxGroupInput("new_user_role", "User Role:", choices = c(
            "Clocking Employee", 
            "Project Manager", 
            "Admin - Purchases", 
            "Admin - Invoicing",
            "Main Director",
            "Accounts"
          ), selected = "Clocking Employee"),
          textInput("new_user_number", "Contact Number:", "27", width = "270px"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("submit_contact", "Submit")
          )
        ))
      })
      
      observeEvent(input$removeEmployee, {
        if (is.null(input$active_empl_table_rows_selected)) showNotification("You need to select at least one row.", type = "error")
        else {
          showModal(modalDialog(
            title = "Are you sure you want to remove all selected users?",
            footer = tagList(
              modalButton("Cancel"),
              actionButton("remove_users", "Proceed")
            )
          ))
        }
      })
      
      observeEvent(input$remove_users, {
        userid = c()
        for (i in input$active_empl_table_rows_selected) {
          userid <- c(userid, enabled_users()[i,]$userid)
        }
        execute(paste0("DELETE FROM active_ts WHERE userid IN (",paste(userid, collapse = ", "),")"))
        enabled_users(get_query("SELECT * FROM active_ts"))
        removeModal()
      })
      
      observeEvent(input$submit_contact, {
        userid <- (sage %>% filter(FirstName == input$new_user_name))$ID
        
        # Roles
        clock <- ifelse("Clocking Employee" %in% input$new_user_role, "true", "false")
        pm <- ifelse("Project Manager" %in% input$new_user_role, "true", "false")
        md <- ifelse("Main Director" %in% input$new_user_role, "true", "false")
        ac <- ifelse("Accounts" %in% input$new_user_role, "true", "false")
        ap <- ifelse("Admin - Purchases" %in% input$new_user_role, "true", "false")
        ai <- ifelse("Admin - Invoicing" %in% input$new_user_role, "true", "false")
        
        execute(paste0("INSERT INTO active_ts (name, wa_number, userid, clock, pm, md, ac, ap, ai) VALUES ('", 
                                  input$new_user_name,"', '", input$new_user_number,"','",userid,"','",clock,
                                  "','",pm,"','",md,"','",ac,"','",ap,"','",ai,"');")) 
        
        enabled_users(get_query("SELECT * FROM active_ts"))
        removeModal()
      })
      
      observeEvent(input$active_empl_table_cell_edit, {
        userid <- enabled_users()[input$active_empl_table_cell_edit$row,]$userid
        execute(paste0("UPDATE active_ts SET wa_number = '", input$active_empl_table_cell_edit$value, 
                                  "' WHERE userid = ", userid)) 
      })
    }
  })                           
}
server <- function(input, output, session) {
  
  # Reactive timer to call get_webhooks every 5 seconds
  autoInvalidate <- reactiveTimer(5000)
  # Timer for connection of postgres every 4 mins
  postgresTimer <- reactiveTimer(1000*60*4)
  new_webhooks <- reactiveVal()
  con <- reactiveVal(dbConnect(RPostgres::Postgres(), user = "ywwysyfrlfahov", password = "4cfd33e0e734d01c8a546ea97940c925e5d14ec94c5ab1058b8a8ecb62632fc0", host = "ec2-54-78-142-10.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "dbgth3mnj5lvab"))
  projects <- reactiveVal(get_projects(get_from_api("TimeTrackingProject","GetActiveProjects")))
  tasks_sheet <- reactiveVal()
  orders_data <- reactiveVal()
  
  observe({
    autoInvalidate()
    new_webhooks(get_new_webhooks())
  })
  
  observe({
    postgresTimer()
    dbDisconnect(isolate(con()))
    con(dbConnect(RPostgres::Postgres(), user = "ywwysyfrlfahov", password = "4cfd33e0e734d01c8a546ea97940c925e5d14ec94c5ab1058b8a8ecb62632fc0", host = "ec2-54-78-142-10.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "dbgth3mnj5lvab"))
  })
  
  sheet <- reactiveVal()
  tasks <- reactiveVal()
  
  observeEvent(input$switch_to_clock, {updateTabItems(session, "sidebartabs", selected = "clock")})
  observeEvent(input$switch_to_proj_planner, {updateTabItems(session, "sidebartabs", selected = "project_planner")})
  observeEvent(input$switch_to_tl_overview, {updateTabItems(session, "sidebartabs", selected = "timeline_overview")})
  observeEvent(input$switch_to_proj_admin, {updateTabItems(session, "sidebartabs", selected = "project_admin")})
  observeEvent(input$switch_to_user_management, {updateTabItems(session, "sidebartabs", selected = "user_management")})
  observeEvent(input$switch_to_orders, {updateTabItems(session, "sidebartabs", selected = "purchases")})
  
  # Project Admin page ----
  output$project_admin_table <- renderDT({
    proj_sheet <- dbGetQuery(con(), "SELECT * FROM projects")
    projects(get_projects(get_from_api("TimeTrackingProject","GetActiveProjects")))
    missing_rows <- projects()[!(projects()$ID %in% proj_sheet$projectid), ]
    if (nrow(missing_rows)>0) {
      add <- data.frame(
        "projectid" = as.numeric(missing_rows$ID),
        "projectname" = missing_rows$Name,
        "customer" = missing_rows$Customer,
        "status" = "Not Started",
        "added" = substr(missing_rows$StartDate, 0,10), 
        "lastupdate" = substr(missing_rows$StartDate, 0,10),
        "invoiceno" = NA,
        "images" = NA,
        "notes" = NA
      )
      dbWriteTable(con(), "projects", add, append = TRUE, overwrite = FALSE)
    }
    datatable(proj_sheet %>%
                select(-projectid) %>%
                arrange(desc(projectname)) %>% 
                mutate(images = ifelse(images != "", paste0("<a href='", images,"' target='_blank'>View</a>"), images)),
              colnames = c("Project Name", "Customer", "Status", "Added", "Last Update", "Invoice No.", "Images", "Notes"),
              escape = FALSE)
  })
  
  observeEvent(input$refreshProjAdmin, {
    con(dbConnect(RPostgres::Postgres(), user = "ywwysyfrlfahov", password = "4cfd33e0e734d01c8a546ea97940c925e5d14ec94c5ab1058b8a8ecb62632fc0", host = "ec2-54-78-142-10.eu-west-1.compute.amazonaws.com", port = 5432, dbname = "dbgth3mnj5lvab"))
  })
  
  # Orders page ----
  observeEvent(input$refreshOrders, {
    if (input$include_received) orders_data(dbGetQuery(con(), paste0("SELECT * FROM orders;")))
    else orders_data(dbGetQuery(con(), paste0("SELECT * FROM orders WHERE status != 'Arrived';")))
  })
  
  output$order_table <- renderDT({
    if (input$include_received) orders_data(dbGetQuery(con(), paste0("SELECT * FROM orders;")))
    else orders_data(dbGetQuery(con(), paste0("SELECT * FROM orders WHERE status != 'Arrived';")))
    datatable(orders_data() %>%
                arrange(desc(datesubmitted)) %>%
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
        modalDialog(
          title = "Upload a Quote",
          div(
            style = "display:inline-block",
            fileInput(
              "quote_file",
              "Upload:",
              multiple = TRUE,
              width = "270px"
            )
          ),
          div(
            style = "display:inline-block",
            checkboxInput(
              "get_quote_approval",
              "Submit for approval",
              value = TRUE,
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
              label = "Complete Upload",
              icon = icon("plus"),
              class = "back"
            )
          )
        ) %>% showModal()
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
            div(
              style = "display:inline-block",
              checkboxInput(
                "set_as_ordered",
                "Mark as ordered.",
                value = TRUE,
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
                inputId = "update_order_to_ordered",
                label = "Update Order",
                icon = icon("pen"),
                class = "back"
              )
            )
          ) %>% showModal()
        } else if (orders_selected_row()$status == "Ordered") {
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
    dbExecute(con(), paste0("UPDATE orders SET status = 'Arrived', lastupdate = '",
                            format(Sys.Date(), format = "%d-%m-%Y"),
                            "' WHERE id = ", orders_selected_row()$id, ";"))
    wa_id <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE name = '",orders_selected_row()$submittedby,"';"))$wa_number
    body_params <- list(
      list('type' = 'text', 'text' = orders_selected_row()$id),
      list('type' = 'text', 'text' = orders_selected_row()$itemdescription)
    )
    send_template(wa_id, body_params = body_params, template_name = 'order_arrived', project = orders_selected_row()$project, orderid = orders_selected_row()$id)
    pm <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE pm = 'true';"))$wa_number
    if (pm != wa_id) send_template(pm, body_params = body_params, template_name = 'order_arrived', project = orders_selected_row()$project, orderid = orders_selected_row()$id)
    
    if (input$include_received) orders_data(dbGetQuery(con(), paste0("SELECT * FROM orders;")))
    else orders_data(dbGetQuery(con(), paste0("SELECT * FROM orders WHERE status != 'Arrived';")))
    removeModal()
  })
  
  observeEvent(input$update_order_to_ordered,{
    if (input$set_as_ordered) dbExecute(con(), paste0("UPDATE orders SET status = 'Ordered', lastupdate = '",
                                                      format(Sys.Date(), format = "%d-%m-%Y"), "', courier = '",
                                                      input$order_courier, "', eta = '", format(input$order_eta[1], format = "%d-%m-%Y"),
                                                      "' WHERE id = ", orders_selected_row()$id, ";"))
    else dbExecute(con(), paste0("UPDATE orders SET lastupdate = '",
                                 format(Sys.Date(), format = "%d-%m-%Y"), "', courier = '",
                                 input$order_courier, "', eta = '", format(input$order_eta[1], format = "%d-%m-%Y"),
                                 "' WHERE id = ", orders_selected_row()$id, ";"))
    if (input$include_received) orders_data(dbGetQuery(con(), paste0("SELECT * FROM orders;")))
    else orders_data(dbGetQuery(con(), paste0("SELECT * FROM orders WHERE status != 'Arrived';")))
    removeModal()
  })
  
  observeEvent(input$upload_files, {
    output$file_upload_error <- renderText({"Processing, please wait..."})
    if (nrow(input$quote_file) == 0) output$file_upload_error <- renderText({"No files were selected."})
    else {
      path <- paste0("ProjectQuotes/",substr(orders_selected_row()$project, 1,4),"/")
      try(path <- drive_mkdir(substr(orders_selected_row()$project, 1,4), path = "ProjectQuotes/", overwrite = FALSE))
      for (i in 1:nrow(input$quote_file)) {
        row <- input$quote_file[i, ]
        drive_upload(row$datapath, 
                     name = row$name, 
                     type = row$type, 
                     path = path)
        if (input$get_quote_approval) {
          body = list(
            'messaging_product' = 'whatsapp',
            'file' = upload_file(row$datapath),
            'name' = row$name
          )
          res <- VERB("POST", url = "https://graph.facebook.com/v18.0/206349329226876/media", body = body, add_headers("Authorization" = paste0("Bearer ",wa_token)), encode = 'multipart')
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
          md <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE md = 'true';"))$wa_number
          message <- send_template(md, body_params, "quote_approval", header, project=orders_selected_row()$project, orderid = orders_selected_row()$id, docid = docID)
          if (!message) output$file_upload_error <- renderText({"Unable to send the quotes for approval."})
        }
      }
      if (input$get_quote_approval) dbExecute(con(), paste0("UPDATE orders SET status = 'Quotes Uploaded' WHERE id = ",orders_selected_row()$id,";" ))
      
      folder <- drive_find(pattern = substr(orders_selected_row()$project, 1,4), type = "folder")
      dbExecute(con(), paste0("UPDATE orders SET quotes = '",folder$drive_resource[[1]]$webViewLink, "' WHERE id = ",orders_selected_row()$id,";" ))
      if (input$include_received) orders_data(dbGetQuery(con(), paste0("SELECT * FROM orders;")))
      else orders_data(dbGetQuery(con(), paste0("SELECT * FROM orders WHERE status != 'Arrived';")))
      removeModal()
    }
  })
  
  observeEvent(input$addOrder, {
    active_employees <- dbGetQuery(con(), "SELECT name FROM active_ts")$name
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
          choices = c("Lisa", "Deneys", "Ursula", active_employees),
          width = "270px"
        )
      ),
      div(
        style = "display:inline-block",
        selectInput(
          inputId = "new_order_proj",
          label = "Project:",
          choices = dbGetQuery(con(), "SELECT projectname FROM projects WHERE status IN ('Not Started', 'In Progress');")$projectname,
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
    df <- data.frame(
      "text" = paste0("O", substr(input$new_order_proj, 1,4), " ", input$new_order_description),
      "from" = dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE name = '",input$new_order_empl,"';"))$wa_number
    )
    create_order(df)
    if (input$include_received) orders_data(dbGetQuery(con(), paste0("SELECT * FROM orders;")))
    else orders_data(dbGetQuery(con(), paste0("SELECT * FROM orders WHERE status != 'Arrived';")))
    removeModal()
  })
  
  observeEvent(input$dismiss_modal, {
    removeModal()
  })
  
  # Clock page ----
  observeEvent(input$sidebartabs,{
    if (input$sidebartabs == "clock") {
      active_box <- reactiveVal("employees")
      hide("projects")
      hide("tasks_div")
      shinyjs::show("employees")
      
      customers <- reactiveVal(data.frame())
      new_df <-  isolate(sheet())
      timesheet <-  NULL
      current_proj_tasks <- reactiveVal()
      
      output$dt_employees <- renderDT({
        sheet(dbGetQuery(con(), "SELECT * FROM active_ts"))
        dt_employees <- datatable(
          sheet() %>% select(name, customer, project, task, timein),
          selection = "single",
          options = list(
            scrollX = TRUE,
            dom = "t",
            paging = FALSE,
            searching = FALSE
          ),
          rownames = FALSE,
          colnames = c('Name', 'Customer', 'Project', 'Task', 'Time In')
        ) %>%
          formatStyle(column = 'name', lineHeight='250%')
        
      }) # renderDT
      
      # Employee clicked
      observeEvent(input$dt_employees_rows_selected, {
        employee_index <<- input$dt_employees_rows_selected
        rowClicked <- isolate(sheet()[employee_index, ])
        timesheet["TimeTrackingUserId"] <<- rowClicked$userid
        timesheet["User"] <<- rowClicked$name
        timesheet["Comments"] <<- rowClicked$timein
        timesheet["TimeTrackingProjectId"] <<- rowClicked$projectid
        timesheet["TimeTrackingCustomerId"] <<- rowClicked$customerid
        timesheet["TimeTrackingTaskId"] <<- rowClicked$taskid
        shinyjs::hide("employees")
        
        output$dt_projects <- renderDT({
          active_proj <- dbGetQuery(con(), "SELECT projectid FROM projects WHERE status IN ('Not Started', 'In Progress')")$projectid
          projects(projects() %>% filter(ID %in% active_proj))
          # Create Project Table
          datatable(select(projects(),Name, Customer), 
                    selection = 'single',
                    options = list(order = list(list(0, 'desc'))),
                    rownames = FALSE) %>%
            formatStyle(column = 'Name', lineHeight='400%')
        })
        shinyjs::show("projects")
        active_box("projects")
      })
      
      observeEvent(input$backBtnTasks,{
        shinyjs::hide("tasks_div")
        shinyjs::show("projects")
        active_box("projects")
      })
      
      observeEvent(input$backBtn,{
        shinyjs::hide("projects")
        shinyjs::show("employees")
        active_box("employees")
      })
      
      observeEvent(input$refreshClockProjects, {
        projects(get_projects(get_from_api("TimeTrackingProject/GetActiveProjects")))
        proj_sheet <- dbGetQuery(con(), "SELECT * FROM projects")
        missing_rows <- projects()[!(projects()$ID %in% proj_sheet$projectid), ]
        
        if (nrow(missing_rows)>0) {
            md <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE md = 'true';"))$wa_number
          for (i in 1:nrow(missing_rows)) {
            body <- list(
              list("type" = "text", "text" = missing_rows$Name)
            )
            send_template(md, body, template_name = 'md_new_project', project = missing_rows$Name)
          }
          add <- data.frame(
            "projectid" = as.numeric(missing_rows$ID),
            "projectname" = missing_rows$Name,
            "customer" = missing_rows$Customer,
            "status" = "Not Started",
            "added" = substr(missing_rows$StartDate, 0,10), 
            "lastupdate" = substr(missing_rows$StartDate, 0,10),
            "invoiceno" = NA,
            "images" = NA,
            "notes" = NA
          )
          dbWriteTable(con(), "projects", add, append = TRUE, overwrite = FALSE)
        }
        
      })
      
      # Show new project modal
      observeEvent(input$addProject, {
        df <- get_customers()
        colnames(df) <- c("CustomerID", "Customer")
        isolate({
          customers(df)
          modalDialog(
            title = "Add New Project",
            div(
              style = "display:inline-block",
              textInput(
                inputId = "proj_name",
                label = "Project Name:",
                value = get_next_project(projects()),
                width = "270px"
              )
            ),
            div(
              style = "display:inline-block",
              selectInput(
                inputId = "proj_cust",
                label = "Customer:",
                choices = c("[New Customer]", customers()$Customer),
                selected = customers()$Customer[1],
                width = "270px"
              )
            ),
            div(
              id = "new_customer_div",
              style = "display:inline-block",
              textInput(
                inputId = "new_customer",
                label = "New Customer Name:",
                value = "",
                width = "270px"
              )
            ),
            size = "s",
            easyClose = FALSE,
            footer = div(
              class = "pull-right container",
              fluidRow(textOutput("add_proj_error")),
              actionButton(
                inputId = "dismiss_modal",
                label = "Close",
                icon = icon("xmark"),
                class = "add_proj"
              ),
              actionButton(
                inputId = "confirm_add",
                label = "Add Project",
                icon = icon("plus"),
                class = "back"
              )
            )
          ) %>% showModal()  
          shinyjs::hide("new_customer_div")
        })
      })
      
      observeEvent(input$dismiss_modal, {
        shinyjs::show("projects")
        removeModal()
      })
      
      # New customer
      observeEvent(input$proj_cust, {
        if (input$proj_cust == "[New Customer]") {
          shinyjs::show("new_customer_div")
        }
      })
      
      # Confirm add project clicked
      observeEvent(input$confirm_add, {
        success <- FALSE
        if (input$new_customer != "") {
          s <- post_to_api1("Customer", list("Name" = input$new_customer,
                                             "Active" = 'true'))
          if (is.list(s)) {
            selectedCustomerID <- s$ID
            success <- TRUE
          } else {
            output$add_proj_error <- renderText({s})
          }
        } else {
          success <- TRUE
          selectedCustomerID <- customers() %>%
            filter(Customer == input$proj_cust) %>%
            select(CustomerID) %>%
            pull()
        }
        if (success) {
          p <- post_to_api1("TimeTrackingProject", list("TimeTrackingProject" = 
                                                          list("Name" = input$proj_name, 
                                                               "CustomerID" = selectedCustomerID,
                                                               "BillingMethodID" = 3,
                                                               "InvoicingMethodID" = 3,
                                                               "StartDate" = format(with_tz(now(tzone = "GMT"), tzone = "Africa/Johannesburg"),"%Y-%m-%d"),
                                                               "ProjectTasks" = list(list("TaskID" = default_task)), # ProjectTasks as a list of lists
                                                               "BudgetBasedOn" = 1,
                                                               "Active" = "true")))
          if (is.list(p)) {
            removeModal()
            showNotification("Project successfully added")
            projects(get_projects(get_from_api("TimeTrackingProject/GetActiveProjects")))
          } else {
            output$add_proj_error <- renderText({p})
          }
        }
      })
      
      # Clicked on project
      observeEvent(input$dt_projects_rows_selected, {
        if (!is.null(projects())) {
          row <- input$dt_projects_rows_selected
          rowClicked <- projects()[row, ]
          new_df <<- sheet()
          new_df$project[employee_index] <<- rowClicked$Name
          new_df$projectid[employee_index] <<- rowClicked$ID
          new_df$customer[employee_index] <<- rowClicked$Customer
          new_df$customerid[employee_index] <<- rowClicked$CustomerID
          new_df$taskid[employee_index] <<- default_task
          new_df$task[employee_index] <<- "Default"
          
          output$dt_tasks <- renderDT({
            d <- data.frame(
              taskid = 0,
              projectname = rowClicked$Name,
              taskname = "Other",
              description = "Other",
              status = "In Progress",
              employee = timesheet["User"],
              plannedstart = NA,
              plannedcompletion = NA,
              colour = NA
            )
            tasks(bind_rows(d,dbGetQuery(con(), paste0("SELECT * FROM tasks WHERE employee = '", timesheet["User"], "';"))))
            current_proj_tasks(tasks() %>% filter(projectname == rowClicked$Name))
            # Create Task Table
            datatable(current_proj_tasks() %>% select(c("taskname", "description")), 
                      selection = 'single',
                      rownames = FALSE,
                      colnames = c("Task Name", "Description")) %>%
              formatStyle(column = 'taskname', lineHeight='200%')
          })
          shinyjs::hide("projects")
          shinyjs::show("tasks_div")
          active_box("tasks_div")
        }
      })
      
      # Clicked on task
      observeEvent(input$dt_tasks_rows_selected, {
        row <- input$dt_tasks_rows_selected
        rowClicked <- current_proj_tasks()[row, ]
        new_df$taskid[employee_index] <<- task_ids[rowClicked$taskname]
        new_df$task[employee_index] <<- rowClicked$taskname
        if (rowClicked$taskname == "Other") {
          showModal(modalDialog(
            title="Clock In",
            paste0(
              timesheet["User"],
              ", you are about to clock on ",
              new_df$project[employee_index],
              ": ",
              new_df$task[employee_index],
              "."
            ),
            textInput("user_comment", "Please provide a description:"),
            footer = tagList(
              actionButton(
                inputId = "dismiss_modal",
                label = "Cancel",
                icon = icon("xmark"),
                class = "add_proj"
              ),
              actionButton("confirmClock", "Confirm")
            )
          ))
        } else {
          showModal(modalDialog(
            title="Clock In",
            paste0(
              timesheet["User"],
              ", are you sure you want to clock on ",
              new_df$project[employee_index],
              "?"
            ),
            footer = tagList(
              actionButton(
                inputId = "dismiss_clockin_modal",
                label = "Cancel",
                icon = icon("xmark"),
                class = "add_proj"
              ),
              actionButton("confirmClock", "Confirm")
            )
          ))
        }
        
        shinyjs::hide("tasks_div")
        shinyjs::hide("projects")
        shinyjs::show("employees")
        
      })
      
      observeEvent(input$dismiss_clockin_modal, {
        shinyjs::hide("employees")
        shinyjs::show("tasks_div")
        shinyjs::hide("projects")
      })
      
      # Confirm clock in clicked
      observeEvent(input$confirmClock, {
        empl_ts <-  dbGetQuery(con(), paste0("SELECT * FROM active_ts WHERE name = '", 
                                             timesheet["User"], "';"))
        empl_row <- new_df[employee_index,]
        if (!is.na(empl_ts$project)) {
          time_in_datetime <- as.POSIXlt(paste(Sys.Date(), empl_ts$timein)) - dhours(2)
          timesheet["Date"] <- format(with_tz(now(tzone = "GMT"), tzone = "Africa/Johannesburg"),"%Y-%m-%d")
          timesheet["Hours"] <- as.numeric(as.duration(now(tzone = "Africa/Johannesburg") - time_in_datetime), "hours")
          timesheet["Comments"] <- paste(timesheet["Comments"], "-", format(with_tz(now(tzone = "GMT"), tzone = "Africa/Johannesburg"), format = "%H:%M"), ifelse(is.na(empl_ts$comment), "", empl_ts$comment))
          post_to_api("TimeTrackingTimesheet",timesheet)
        }
        comment <- ifelse(is.null(input$user_comment),"", input$user_comment) 
        timein <- format(with_tz(now(tzone = "GMT"), tzone = "Africa/Johannesburg"), format = "%H:%M")
        dbExecute(con(), paste0("UPDATE active_ts SET project = '",empl_row$project,"', projectid = ",
                                empl_row$projectid,", task = '",empl_row$task,"', taskid = ",empl_row$taskid,
                                ", customer = '",empl_row$customer,"', customerid = ",empl_row$customerid,
                                ", timein = '", timein,"', comment = '",comment,"' WHERE name = '",timesheet["User"],"';"))
        
        showNotification("Timesheet successfully updated.")
        print(paste("IN:", empl_row$name, empl_row$project, empl_row$timein))
        sheet(dbGetQuery(con(), paste0("SELECT * FROM active_ts")))
        removeModal()
        shinyjs::hide("projects")
        shinyjs::show("employees")
        active_box("employees")
        taskRow <- current_proj_tasks()[input$dt_tasks_rows_selected, ]
        status <- dbGetQuery(con(), paste0("SELECT status FROM tasks WHERE taskid = ", input$dt_tasks_rows_selected))$status
        if (length(status)>0 && status == "Not Started" && taskRow$taskid != 0) {
          body <- list(
            list("type" = "text", "text" = empl_row$name), 
            list("type" = "text", "text" = paste0(taskRow$taskname, ": ", taskRow$description)), 
            list("type" = "text", "text" = format(as.POSIXct(taskRow$plannedcompletion), format = "%H:%M %d %B"))
          )
          send_template(get_wa_id(empl_row$name), body, template_name = 'task_time', heading = as.list(c("type" = "text", "text" = taskRow$projectname)), taskid = taskRow$taskid) 
          if (dbGetQuery(con(), paste0("SELECT status FROM projects WHERE projectid = ",empl_row$projectid,";"))$status == "Not Started") {
            md <- dbGetQuery(con(), paste0("SELECT wa_number FROM active_ts WHERE md = 'true';"))$wa_number
            body <- list(
              list("type" = "text", "text" = empl_row$project)
            )
            send_template(md, body, template_name = 'md_project_started', project = empl_row$project)
          }
          dbExecute(con(), paste0("UPDATE tasks SET status = 'In Progress' WHERE taskid = ",empl_row$taskid,";"))
          dbExecute(con(), paste0("UPDATE projects SET status = 'In Progress' WHERE projectid = ",empl_row$projectid,";"))
        }
      })
      
      # Confirm Breakfast is clicked
      observeEvent(input$confirmBreakfast, {
        empl_ts <-  dbGetQuery(con(), paste0("SELECT * FROM active_ts WHERE name = '", 
                                             timesheet["User"], "';"))
        if (!is.na(empl_ts$project)) {
          time_in_datetime <- as.POSIXlt(paste(Sys.Date(), empl_ts$timein)) - dhours(2)
          timesheet["Date"] <- format(with_tz(now(tzone = "GMT"), tzone = "Africa/Johannesburg"),"%Y-%m-%d")
          timesheet["Hours"] <- as.numeric(as.duration(now(tzone = "Africa/Johannesburg") - time_in_datetime), "hours")
          timesheet["Comments"] <- paste(timesheet["Comments"], "-", format(with_tz(now(tzone = "GMT"), tzone = "Africa/Johannesburg"), format = "%H:%M"), ifelse(is.na(empl_ts$comment), "", empl_ts$comment))
          post_to_api("TimeTrackingTimesheet",timesheet)
          
        }
        comment <- ifelse(is.null(input$user_comment),"", input$user_comment) 
        timein <- format(with_tz(now(tzone = "GMT"), tzone = "Africa/Johannesburg"), format = "%H:%M")
        dbExecute(con(), paste0("UPDATE active_ts SET project = '0000 - Svenco Engineering', projectid = 290320, 
            task = 'Breakfast Break', taskid = ",breakfast_task,", customer = 'Svenco Engineering', customerid = 31662927, timein = '", timein,"', 
            comment = '",comment,"' WHERE name = '",timesheet["User"],"';"))
        showNotification("Timesheet successfully updated.")
        sheet(dbGetQuery(con(), paste0("SELECT * FROM active_ts")))
        removeModal()
        shinyjs::hide("projects")
        shinyjs::show("employees")
        active_box("employees")
      })
      
      
      # Clock Out ----
      
      # Open clock out modal
      observeEvent(list(input$clockOutBtn,input$lunchBtn), {
        req(input$clockOutBtn | input$lunchBtn)
        showModal(modalDialog(
          title="Clock Out",
          paste0(
            timesheet["User"],
            ", are you sure you want to clock out?"
          ),
          footer = tagList(actionButton(
            inputId = "dismiss_modal",
            label = "Cancel",
            icon = icon("xmark"),
            class = "add_proj"
          ),
          actionButton("confirmClockOut", "Confirm")
          )
        ))
      })
      
      # Open break modal
      observeEvent(input$breakBtn, {
        showModal(modalDialog(
          title="Breakfast Break",
          paste0(
            timesheet["User"],
            ", are you sure you want to take your breakfast break?"
          ),
          footer = tagList(actionButton(
            inputId = "dismiss_modal",
            label = "Cancel",
            icon = icon("xmark"),
            class = "add_proj"
          ),
          actionButton("confirmBreakfast", "Confirm")
          )
        ))
      })
      
      # Clockout button clicked
      observeEvent(input$confirmClockOut, {
        empl_ts <-  dbGetQuery(con(), paste0("SELECT * FROM active_ts WHERE name = '", 
                                             timesheet["User"], "';"))
        if (!is.na(empl_ts$project)) {
          time_in_datetime <- as.POSIXlt(paste(Sys.Date(), empl_ts$timein)) - dhours(2)
          timesheet["Date"] <- format(with_tz(now(tzone = "GMT"), tzone = "Africa/Johannesburg"),"%Y-%m-%d")
          timesheet["Hours"] <- as.numeric(as.duration(now(tzone = "Africa/Johannesburg") - time_in_datetime), "hours")
          timesheet["Comments"] <- paste(timesheet["Comments"], "-", format(with_tz(now(tzone = "GMT"), tzone = "Africa/Johannesburg"), format = "%H:%M"), ifelse(is.na(empl_ts$comment), "", empl_ts$comment))
          post_to_api("TimeTrackingTimesheet",timesheet)
          dbExecute(con(), paste0("UPDATE active_ts SET project = NULL, projectid = NULL, 
            task = NULL, taskid = NULL, customer = NULL, customerid = NULL, timein = NULL, 
            comment = NULL WHERE name = '",timesheet["User"],"';"))
        }
        
        showNotification("Timesheet successfully updated.")
        print(paste("OUT:", new_df$Name[employee_index], new_df$Project[employee_index], timesheet["Comments"]))
        sheet(dbGetQuery(con(), paste0("SELECT * FROM active_ts")))
        removeModal()
        shinyjs::hide("projects")
        shinyjs::show("employees")
        active_box("employees")
      })
    }
  }) # Observe - clock page
  
  # Project Management -----
  observeEvent(input$sidebartabs,{
    
    if (input$sidebartabs == "project_planner") {
      tasks_sheet(dbGetQuery(con(), "SELECT * FROM tasks"))
      proj_timevis_data <- reactiveVal(data.frame())
      tasks <- reactiveVal(get_tasks(get_from_api("TimeTrackingTask","Get","$filter=Active eq true&includeProjectTasks=True&")))
      updateSelectizeInput(session, "proj_plan", label = NULL, choices=c("Select Project", sort(projects()$Name, decreasing = TRUE)))
      
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
        enabled_users <- dbGetQuery(con(), "SELECT name FROM active_ts")$name
        proj_id <- (projects() %>% filter(Name == input$proj_plan))$ID
        tasks <- filter(tasks(), grepl(paste0("\\b", proj_id, "\\b"), ProjectID))$Name
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
        proj_id <- (projects() %>% filter(Name == input$proj_plan))$ID
        current_proj <- input$proj_plan
        if (!is.null(current_proj) && current_proj != "") {
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
        new_id <- paste(nrow(tasks_sheet()) + 1)
        new_item <- data.frame(
          taskid = new_id,
          projectname = input$proj_plan,
          taskname = input$task_name,
          description = input$task_description,
          status = "Not Started",
          employee = input$new_task_empl,
          plannedstart = paste(format(input$task_dates[1], format = "%Y-%m-%d"), format(input$task_start_time, format = "%H:%M:%S")),
          plannedcompletion = paste(format(input$task_dates[2], format = "%Y-%m-%d"), format(input$task_end_time, format = "%H:%M:%S")),
          colour = task_colors[input$task_name]
        )
        
        dbExecute(con(), paste0("INSERT INTO tasks (taskid, projectname, taskname, description, status, employee, 
                              plannedstart, plannedcompletion, colour) VALUES (", new_item$taskid, ",'",new_item$projectname,
                                "','", new_item$taskname, "', '", new_item$description,"', '", new_item$status, "', '", 
                                new_item$employee,  "', '", new_item$plannedstart, "', '", 
                                new_item$plannedcompletion, "', '", new_item$colour, "');"))
        tasks_sheet(dbGetQuery(con(), "SELECT * FROM tasks"))
        proj_timevis_data(updateProjTimevis(tasks_sheet() %>% filter(projectname == input$proj_plan)))
        shiny::removeModal()
      })
      
      observeEvent(input$projectTimeVis_selected, {
        selected_task <- dbGetQuery(isolate(con()), paste0("SELECT * FROM tasks WHERE taskid = ", input$projectTimeVis_selected, ";"))
        enabled_users <- dbGetQuery(isolate(con()), paste0("SELECT name FROM active_ts;"))$name
        proj_id <- (isolate(projects()) %>% filter(Name == input$proj_plan))$ID
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
        dbExecute(con(), paste0("UPDATE tasks SET taskname = '",input$edit_task_name,"', description = '",input$edit_task_description,
                                "', status = '",input$edit_task_status,"', employee = '",input$edit_task_empl,
                                "', plannedstart = '",paste(format(input$edit_task_dates[1], format = "%Y-%m-%d"), format(input$edit_task_start_time, format = "%H:%M:%S")),
                                "', plannedcompletion = '",paste(format(input$edit_task_dates[2], format = "%Y-%m-%d"), format(input$edit_task_end_time, format = "%H:%M:%S")),
                                "', colour = '",task_colors[input$edit_task_name],"' WHERE taskid = ", input$projectTimeVis_selected,";"))
        if (input$edit_task_status == "In Progress")  dbExecute(con(), paste0("UPDATE projects SET status = 'In Progress' WHERE projectname = '", input$proj_plan,"';"))
        if (input$edit_task_status == "QC Passed") {
          df <- data.frame(
            "task" = input$projectTimeVis_selected,
            "project" = input$proj_plan
          )
          task_QC_passed(df)
        }
        if (input$edit_task_status == "Completed") tasks_ready_for_QC(input$projectTimeVis_selected)
        proj_timevis_data(updateProjTimevis(dbGetQuery(con(), paste0("SELECT * FROM tasks WHERE projectname = '",input$proj_plan, "';")))) 
        removeModal()
      })
      
      observeEvent(input$dltTaskProj, {
        dbExecute(con(), paste0("DELETE FROM tasks WHERE taskid = ", projectTimeVis_selected,";"))
        proj_timevis_data(dbGetQuery(con(), paste0("SELECT * FROM tasks WHERE projectname = '",input$proj_plan, "';")))
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
      tasks_sheet(dbGetQuery(con(), "SELECT * FROM tasks"))
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
      id = unique(df$projectname),
      content = unique(df$projectname)
    )
    if (nrow(df)>0) {
      timevis_data <- data.frame(
        content = templateTask(df$taskname, df$description,df$employee,df$status),
        id = df$taskid,
        start = as.POSIXct(df$plannedstart),
        end = as.POSIXct(df$plannedcompletion),
        group = df$projectname,
        style = paste("background-color:", df$colour, "; font-size:5px")
      )
    } else timevis_data <- data.frame()
    
    timevis(data = timevis_data, groups = groups) %>%
      setWindow(input$tl_ov_dates[1], input$tl_ov_dates[2]+1)
  })
  
  # User management page - postgres ----
  observeEvent(input$sidebartabs,{
    
    if (input$sidebartabs == "user_management") {
      sage <- get_users(get_from_api("TimeTrackingUser"))
      enabled_users <- reactiveVal(dbGetQuery(con(), "SELECT * FROM active_ts"))
      
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
        dbExecute(con(), paste0("DELETE FROM active_ts WHERE userid IN (",paste(userid, collapse = ", "),")"))
        enabled_users(dbGetQuery(con(), "SELECT * FROM active_ts"))
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
        
        dbExecute(con(), paste0("INSERT INTO active_ts (name, wa_number, userid, clock, pm, md, ac, ap, ai) VALUES ('", 
                                input$new_user_name,"', '", input$new_user_number,"','",userid,"','",clock,
                                "','",pm,"','",md,"','",ac,"','",ap,"','",ai,"');"))
        enabled_users(dbGetQuery(con(), "SELECT * FROM active_ts"))
        removeModal()
      })
      
      observeEvent(input$active_empl_table_cell_edit, {
        userid <- enabled_users()[input$active_empl_table_cell_edit$row,]$userid
        dbExecute(con(), paste0("UPDATE active_ts SET wa_number = '", input$active_empl_table_cell_edit$value, 
                                "' WHERE userid = ", userid))
      })
    }
  })                           
}
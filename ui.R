dashboardPage(
  
  title = "Project Manager",
  
  dashboardHeader(
    title = "Project Manager"
  ),
  
  dashboardSidebar(sidebarMenu(
                     id = "sidebartabs",
                     menuItem("Project Admin", tabName = "project_admin", icon = icon("list-check")),
                     menuItem("Timeline Overview",tabName = "timeline_overview", icon = icon("timeline")),
                     menuItem("Project Planner", tabName = "project_planner", icon = icon("calendar-days")),
                     menuItem("Purchases", tabName = "purchases", icon = icon("cart-shopping")),
                     menuItem("User Management", tabName = "user_management", icon = icon("users")),
                     menuItem("Project Report", tabName = "project_report", icon = icon("chart-line"))
                   )
  ),
  
  dashboardBody(
    useShinyjs(),
    tagList(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      )
    ),
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.png"),
      tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.png"),
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.pngg"),
      tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png")
    ),
    tabItems(
      tabItem(
        tabName = "project_planner",
        fluidRow(
          div(
            column(width = 4, div(class = "custom-selectize", selectizeInput(width = "400px", "proj_plan", label = NULL, choices=c("Loading...")))),
            #selectizeInput(width = "250px", "proj_plan", label = NULL, choices=c("Loading...")),
            column(width = 1, actionButton(inputId = "projAdminHelp", label = "",icon = icon("question"), class = "back", 
                                           onclick = "window.open('https://docs.google.com/document/d/1-xM7oF17Uzbx4fKCS0hfL2pqOU9pGMmshOmE0wuZFC8/edit?usp=sharing', '_blank')"),),
            column(width = 5, actionButton(inputId = "refreshProjects", label = "",icon = icon("rotate-right"), class = "back")),
            #actionButton(inputId = "addTask", label = "Add Task",icon = icon("plus"), class = "add_proj")
            column(width = 2, actionButton(inputId = "addTask", label = "Add Task",icon = icon("plus"), class = "add_proj"))
          )),
        fluidRow(div(
          id = "projectTasks_div",
          style = "display: none;",
          box(
            title = "Project Tasks", 
            width = NULL,
            timevisOutput("projectTimeVis")
          )
        ))
      ),
      tabItem(
        tabName = "timeline_overview",
        box(
          title = "Applied Filters",
          width = NULL,
          column(
            width = 3,
            selectizeInput("tl_ov_employees", "Employee", c("All"))
          ),
          column(
            width = 3,
            selectizeInput("tl_ov_project", "Project", c("All"))
          ),
          column(
            width = 3,
            selectizeInput("tl_ov_task", "Task", c("All"))
          ),
          column(
            width = 3,
            daterangepicker(
              "tl_ov_dates",
              "Date Range",
              Sys.Date(),
              Sys.Date(),
              style = "border:1px solid lightgrey; height:32px; width:210px; text-align: center;"
            )
          )
        ),
        box(
          title = "Timeline Overview",
          width = NULL,
          timevisOutput("overviewTimeVis")
        )
      ),
      tabItem(
        tabName = "user_management",
        fluidRow(
          div(
            actionButton(inputId = "employeesHelp", label = "",icon = icon("question"), class = "back", 
                         onclick = "window.open('https://docs.google.com/document/d/1wyPcBK808MOiV8MT9InUljp16fdqA6CLcCrAUj-hLDk/edit?usp=sharing', '_blank')"),
            actionButton(inputId = "removeEmployee", label = "Remove", icon = icon("trash"), class = "add_proj"),
            actionButton(inputId = "addEmployee", label = "Add", icon = icon("plus"), class = "add_proj")
          )),
        fluidRow(div(
          box(
            title = "Active Employees",
            DTOutput("active_empl_table"),
            width = NULL
          )
        ))
      ),
      tabItem(
        tabName = "project_report",
          div(fluidRow( box(width=12,style="padding:0px;",
            column(width=4,selectInput("report_inv", "", c("Loading..."))),
            column(width=4,selectInput("report_ref", "", c("Loading..."))),
            column(width = 4,
                   br(),
                   column(width=6,actionButton(inputId = "generate_report", label = "Generate Report", class = "back")),
                   column(width=6,actionButton(inputId = "refreshProjectReport", label = "Refresh Project Data", icon = icon("rotate-right"), class = "add_proj"))
            )
          ))),
          div(id = "proj_report_div", style = "display:none;",
              fluidRow(
                valueBoxOutput("total_labour_profit"),
                valueBoxOutput("total_material_profit"),
                valueBoxOutput("total_profit")
              ),
              fluidRow(
                valueBoxOutput("total_material_cost", width = 3),
                valueBoxOutput("total_material_charged", width = 3),
                valueBoxOutput("total_labour_cost", width = 3),
                valueBoxOutput("total_labour_charged", width = 3)
              ),
              fluidRow(
                box(width = 12, title = "Material Invoiced", collapsible = TRUE,
                  DTOutput("dt_invoice_material")
                )
              ),
              fluidRow(
                box(width = 7, title = "Labour Invoiced", collapsible = TRUE,
                  DTOutput("dt_invoice_labour")
                ),
                box(width = 5, title = "Actual Labour", collapsible = TRUE,
                  DTOutput("dt_project_labour")
                )
              )
            )
          ),
      tabItem(
        tabName = "project_admin",
        div(
          actionButton(inputId = "refreshProjAdmin", label = "",icon = icon("rotate-right"), class = "back"),
          actionButton(inputId = "projAdminHelp", label = "",icon = icon("question"), class = "back", 
                       onclick = "window.open('https://docs.google.com/document/d/1UjW5uidLSUVQG1h-7FpYQITXSfGcpn3wW9WZ5QKbMvU/edit?usp=sharing', '_blank')"),
          
          actionButton(inputId = "update_proj_status", label = "Update Status", icon = icon("bars-progress"), class = "add_proj")
        ),
        box(
          title = "Project Administration", 
          width = NULL,
          checkboxInput("show_only_incomplete_projects", "Show only projects that still requires action", TRUE),
          DTOutput("project_admin_table")
        )
      ),
      tabItem(
        tabName = "purchases",
        
        fluidRow(
          div(
            actionButton(inputId = "refreshOrders", label = "",icon = icon("rotate-right"), class = "back"),
            actionButton(inputId = "purchasesHelp", label = "",icon = icon("question"), class = "back", 
                         onclick = "window.open('https://docs.google.com/document/d/1ra4syiZASpP8mWvtBBIEc-g69Lu6PzqrpwfQUzynLYA/edit?usp=sharing', '_blank')"),
            actionButton(inputId = "addOrder", label = "New Purchase Request", icon = icon("plus"), class = "add_proj")
          )),
        fluidRow(div(
          box(
            title = "Purchases",
            checkboxInput("include_received", "Include purchases already received.", value = FALSE),
            DTOutput("order_table"),
            width = NULL
          )
        ))
      )
    )
  )
  
)
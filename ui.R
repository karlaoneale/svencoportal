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
                     menuItem("User Management", tabName = "user_management", icon = icon("users"))
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
            column(width = 2, div(class = "custom-selectize", selectizeInput(width = "250px", "proj_plan", label = NULL, choices=c("Loading...")))),
            #selectizeInput(width = "250px", "proj_plan", label = NULL, choices=c("Loading...")),
            column(width = 1, actionButton(inputId = "projAdminHelp", label = "",icon = icon("question"), class = "back", 
                                           onclick = "window.open('https://docs.google.com/document/d/1-xM7oF17Uzbx4fKCS0hfL2pqOU9pGMmshOmE0wuZFC8/edit?usp=sharing', '_blank')"),),
            column(width = 7, actionButton(inputId = "refreshProjects", label = "",icon = icon("rotate-right"), class = "back")),
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
        tabName = "project_admin",
        div(
          actionButton(inputId = "refreshProjAdmin", label = "",icon = icon("rotate-right"), class = "back"),
          actionButton(inputId = "projAdminHelp", label = "",icon = icon("question"), class = "back", 
                       onclick = "window.open('https://docs.google.com/document/d/1UjW5uidLSUVQG1h-7FpYQITXSfGcpn3wW9WZ5QKbMvU/edit?usp=sharing', '_blank')"),
        ),
        box(
          title = "Project Administration", 
          width = NULL,
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
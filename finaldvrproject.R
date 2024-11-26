# Load necessary libraries
library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(bslib)
library(DT)
library(plotly)
library(shinydashboard)
library(waiter)
library(sodium)  # For password hashing
library(RSQLite)  # For user database
library(tidyr)   # For data manipulation in summary statistics
library(caret)   # For machine learning models
library(randomForest)  # For random forest prediction
library(tools)   # For file extension checking
library(lubridate)

# Initialize user database
init_user_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), "users.db")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS users (
    username TEXT PRIMARY KEY,
    password TEXT NOT NULL,
    email TEXT UNIQUE NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
  )")
  dbDisconnect(con)
}

# Register a new user
register_user <- function(username, password, email) {
  # Hash the password
  hashed_password <- hash_password(password)
  
  con <- dbConnect(RSQLite::SQLite(), "users.db")
  tryCatch({
    # Insert new user
    dbExecute(con, "INSERT INTO users (username, password, email) VALUES (?, ?, ?)", 
              params = list(username, hashed_password, email))
    dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    dbDisconnect(con)
    return(FALSE)
  })
}

# Validate user login
validate_login <- function(username, password) {
  con <- dbConnect(RSQLite::SQLite(), "users.db")
  tryCatch({
    # Retrieve hashed password
    result <- dbGetQuery(con, "SELECT password FROM users WHERE username = ?", 
                         params = list(username))
    dbDisconnect(con)
    
    # Check if user exists and password matches
    if (nrow(result) > 0) {
      return(verify_password(result$password, password))
    }
    FALSE
  }, error = function(e) {
    dbDisconnect(con)
    FALSE
  })
}

# Hash password securely
hash_password <- function(password) {
  # Use sodium for secure password hashing
  sodium::password_store(password)
}

# Verify password
verify_password <- function(stored_hash, input_password) {
  tryCatch({
    sodium::password_verify(stored_hash, input_password)
  }, error = function(e) {
    FALSE
  })
}

# Load default dataset
default_data <- read_excel("eca.xlsx")

# Create a custom theme
custom_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#3498db",
  secondary = "#2ec",
  font_scale = 1.1
)

# Login Page UI
login_page <- function() {
  fluidPage(
    theme = custom_theme,
    tags$head(
      tags$style(HTML("
        .login-container {
          display: flex;
          align-items: center;
          justify-content: center;
          height: 100vh;
          background: linear-gradient(135deg, #f6f8f9 0%, #e5ebee 100%);
        }
        .login-box {
          width: 400px;
          padding: 40px;
          background-color: white;
          border-radius: 15px;
          box-shadow: 0 10px 25px rgba(0,0,0,0.1);
        }
        .login-title {
          text-align: center;
          color: #2c3e50;
          margin-bottom: 30px;
        }
        .login-form .form-group {
          margin-bottom: 20px;
        }
        .login-btn {
          width: 100%;
          padding: 12px;
          border-radius: 50px;
        }
        .register-link {
          text-align: center;
          margin-top: 20px;
        }
      "))
    ),
    div(class = "login-container",
        div(class = "login-box",
            h2(class = "login-title", "ECA Data Explorer"),
            div(class = "login-form",
                textInput("login_username", "Username", placeholder = "Enter your username"),
                passwordInput("login_password", "Password", placeholder = "Enter your password"),
                actionButton("login_btn", "Login", class = "btn btn-primary login-btn"),
                div(class = "register-link",
                    p("Don't have an account?", 
                      actionLink("show_register", "Register here"))
                )
            )
        )
    )
  )
}

# Registration Page UI
registration_page <- function() {
  fluidPage(
    theme = custom_theme,
    tags$head(
      tags$style(HTML("
        .register-container {
          display: flex;
          align-items: center;
          justify-content: center;
          height: 100vh;
          background: linear-gradient(135deg, #f6f8f9 0%, #e5ebee 100%);
        }
        .register-box {
          width: 400px;
          padding: 40px;
          background-color: white;
          border-radius: 15px;
          box-shadow: 0 10px 25px rgba(0,0,0,0.1);
        }
        .register-title {
          text-align: center;
          color: #2c3e50;
          margin-bottom: 30px;
        }
        .register-form .form-group {
          margin-bottom: 20px;
        }
        .register-btn {
          width: 100%;
          padding: 12px;
          border-radius: 50px;
        }
        .login-link {
          text-align: center;
          margin-top: 20px;
        }
      "))
    ),
    div(class = "register-container",
        div(class = "register-box",
            h2(class = "register-title", "Create Account"),
            div(class = "register-form",
                textInput("reg_username", "Username", placeholder = "Choose a username"),
                textInput("reg_email", "Email", placeholder = "Enter your email"),
                passwordInput("reg_password", "Password", placeholder = "Create a password"),
                passwordInput("reg_confirm_password", "Confirm Password", placeholder = "Confirm your password"),
                actionButton("register_btn", "Register", class = "btn btn-success register-btn"),
                div(class = "login-link",
                    p("Already have an account?", 
                      actionLink("show_login", "Login here"))
                )
            )
        )
    )
  )
}

# Landing Page UI
landing_page <- function() {
  fluidPage(
    theme = custom_theme,
    tags$head(
      tags$style(HTML("
        .landing-container {
          display: flex;
          align-items: center;
          justify-content: center;
          height: 100vh;
          background: linear-gradient(135deg, #f6f8f9 0%, #e5ebee 100%);
        }
        .landing-content {
          text-align: center;
          max-width: 1200px;
          padding: 200px;
          background-color: white;
          border-radius: 15px;
          box-shadow: 0 10px 25px rgba(0,0,0,0.1);
        }
        .landing-title {
          color: #2c3e50;
          font-weight: bold;
          margin-bottom: 20px;
        }
        .landing-description {
          color: #7f8c8d;
          margin-bottom: 30px;
        }
        .get-started-btn {
          padding: 12px 30px;
          font-size: 18px;
          border-radius: 50px;
          transition: all 0.3s ease;
        }
        .get-started-btn:hover {
          transform: scale(1.05);
          box-shadow: 0 5px 15px rgba(0,0,0,0.1);
        }
        .feature-icons {
          display: flex;
          justify-content: center;
          margin-top: 30px;
        }
        .feature-item {
          margin: 0 20px;
          text-align: center;
          color: #3498db;
        }
      "))
    ),
    div(class = "landing-container",
        div(class = "landing-content",
            h1(class = "landing-title", "ECA Data Explorer"),
            p(class = "landing-description", 
              "A comprehensive platform for analyzing Energy Consumption datasets. 
              Explore global trends, perform advanced statistical analysis, and generate predictive insights 
              to support Energy Consumption research and policy-making."),
            
            # Enhanced feature highlights
            div(class = "feature-icons",
                div(class = "feature-item",
                    tags$i(class = "fas fa-chart-line fa-3x"),
                    p("Advanced Data Visualization"),
                    tags$small("Interactive charts and graphs")
                ),
                div(class = "feature-item",
                    tags$i(class = "fas fa-filter fa-3x"),
                    p("Flexible Data Exploration"),
                    tags$small("Filter by year, country, and variables")
                ),
                div(class = "feature-item",
                    tags$i(class = "fas fa-brain fa-3x"),
                    p("Predictive Modeling"),
                    tags$small("Machine learning predictions")
                ),
                div(class = "feature-item",
                    tags$i(class = "fas fa-chart-pie fa-3x"),
                    p("Comprehensive Analytics"),
                    tags$small("Summary statistics and insights")
                )
            ),
            
            actionButton("start_app", "Explore Data", 
                         class = "btn btn-primary btn-lg get-started-btn mt-4", 
                         icon = icon("rocket"))
        )
    )
  )
}

# Main App UI
main_app_ui <- function() {
  page_fluid(
    theme = custom_theme,
    
    # Custom CSS for styling
    tags$head(
      tags$style(HTML("
        .card {
          margin-bottom: 20px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          border-radius: 01px;
        }
        .nav-pills .nav-link.active {
          background-color: #3498db !important;
        }
        .page-header {
          background-color: #f8f9fa;
          padding: 20px;
          margin-bottom: 20px;
          border-radius: 10px;
        }
        .logout-btn {
          position: absolute;
          top: 15px;
          right: 15px;
        }
        .prediction-results {
          margin-top: 20px;
          padding: 15px;
          background-color: #f8f9fa;
          border-radius: 10px;
        }
      "))
    ),
    
    # Logout button
    actionButton("logout_btn", "Logout", 
                 icon = icon("sign-out-alt"), 
                 class = "btn btn-danger logout-btn"),
    
    # Page title
    div(class = "page-header",
        h1("ECA Data Explorer", class = "text-center"),
        p("Interactive Data Analysis Platform", class = "text-center text-muted")
    ),
    
    # Navigation
    # Home Tab
    navset_card_pill(
    nav_panel("Home", 
              fluidRow(
                box(
                  width = 20,
                  status = "info",
                  solidHeader = TRUE,
                  
                  h2("Welcome to ECA Data Explorer", align = "center"),
                  h3("K Features of this Website:"),
                  tags$ul(
                    tags$li("Real-time data filtering and visualization capabilities"),
                    tags$li("Advanced statistical analysis and trend identification"),
                    tags$li("Predictive modeling using machine learning algorithms"),
                    tags$li("Comparative analysis across different countries and time periods"),
                    tags$li("Interactive charts and customizable visualizations")
                  ),
                  h4("U The navigation menu to explore different aspects of the energy consumption data.")
                )
              ),
              fluidRow(
                valueBoxOutput("total_countries", width = 3),
                valueBoxOutput("year_range", width = 3),
                valueBoxOutput("avg_consumption", width = 3),
                valueBoxOutput("total_records", width = 3)
              ),
              fluidRow(
                box(
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  title = "3D Energy Consumption Visualization",
                  fluidRow(
                    column(4,
                           selectInput("x_var", "X Variable:", choices = NULL),
                           selectInput("y_var", "Y Variable:", choices = NULL),
                           selectInput("z_var", "Z Variable:", choices = NULL),
                           sliderInput("point_size", "Point Size:", 
                                       min = 1, max = 10, value = 3),
                           sliderInput("opacity", "Point Opacity:", 
                                       min = 0.1, max = 1, value = 0.7, step = 0.1)
                    ),
                    column(8,
                           plotlyOutput("scatter_3d", height = "600px")
                    )
                  )
                )
              )
    ),
    
    # Upload Data Tab
    nav_panel("Upload Data", 
              fluidRow(
                box(
                  title = "Upload Data File",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fileInput("file", "Choose Excel/CSV File",
                            multiple = FALSE,
                            accept = c(".xlsx", ".xls", ".csv")),
                  radioButtons("file_type", "File Type",
                               choices = c("Excel" = "excel", "CSV" = "csv"),
                               selected = "excel"),
                  conditionalPanel(
                    condition = "input.file_type == 'csv'",
                    checkboxInput("header", "Header", TRUE),
                    radioButtons("sep", "Separator",
                                 choices = c(Comma = ",", 
                                             Semicolon = ";", 
                                             Tab = "\t"),
                                 selected = ",")
                  ),
                  actionButton("load_data", "Load Data", 
                               class = "btn btn-primary"),
                  br(), br(),
                  textOutput("upload_status"),
                  DTOutput("preview_data")
                )
              )
    ),
    
    # Data Overview Tab
    nav_panel("Data Overview",
              fluidRow(
                box(title = "Advanced Filters", width = 4, status = "primary", solidHeader = TRUE,
                    sliderInput("year", "Select Year Range:", 
                                min = 1990, max = 2020,
                                value = c(1990, 2020), step = 1),
                    selectizeInput("country", "Select Countries:", 
                                   choices = NULL, selected = NULL, multiple = TRUE),
                    sliderInput("consumption_range", "Biofuel Consumption Range:",
                                min = 0, max = 100, value = c(0, 100)),
                    selectInput("sort_by", "Sort By:", choices = NULL),
                    selectInput("sort_order", "Sort Order:",
                                choices = c("Ascending" = "asc", "Descending" = "desc"),
                                selected = "asc"),
                    actionButton("reset", "Reset Filters", 
                                 class = "btn btn-secondary btn-block", 
                                 icon = icon("refresh"))
                ),
                box(title = "ECA Dataset", width = 8, status = "info", solidHeader = TRUE,
                    DTOutput("table")
                )
              )
    ),
    
    # Data Visualizations Tab
    nav_panel("Data Visualizations",
              fluidRow(
                box(title = "Visualization Controls", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("var_x", "X-axis Variable:", choices = NULL),
                    selectInput("var_y", "Y-axis Variable:", choices = NULL),
                    selectInput("chart_type", "Chart Type:",
                                choices = c("Scatter Plot", "Bar Chart", "Line Chart", "Box Plot", "Heat Map"),
                                selected = "Scatter Plot"),
                    conditionalPanel(
                      condition = "input.chart_type == 'Scatter Plot'",
                      checkboxInput("show_trend", "Show Trend Line", TRUE),
                      selectInput("trend_method", "Trend Line Type:",
                                  choices = c("Linear" = "lm", "Loess" = "loess"),
                                  selected = "lm")
                    ),
                    selectInput("color_scheme", "Color Scheme:",
                                choices = c("Default", "Viridis", "Magma", "Plasma"),
                                selected = "Default"),
                    checkboxInput("log_scale", "Use Log Scale for Y-axis", FALSE),
                    downloadButton("download_plot", "Download Plot", 
                                   class = "btn btn-info btn-block")
                ),
                box(title = "Visualization", width = 8, status = "info", solidHeader = TRUE,
                    plotlyOutput("dynamic_plot", height = "600px")
                )
              )
    ),
    
    # Summary Stats Tab
    nav_panel("Summary Stats",
              fluidRow(
                box(title = "Statistical Summary", 
                    width = 4, 
                    status = "primary", 
                    solidHeader = TRUE,
                    actionButton("calc_stats", "Calculate Stats", 
                                 class = "btn btn-info")
                ),
                box(title = "Summary Statistics Table", 
                    width = 8, 
                    status = "info", 
                    solidHeader = TRUE,
                    div(class = "summary-table-container",
                        tableOutput("summary_table")
                    )
                )
              )
    ),
    
    # Prediction Tab
    nav_panel("Prediction",
              fluidRow(
                box(title = "Model Configuration", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("target_var", "Target Variable:", choices = NULL),
                    checkboxGroupInput("pred_vars", "Predictor Variables:", choices = NULL),
                    numericInput("future_years", "Forecast Horizon (Years):",
                                 value = 5, min = 1, max = 20, step = 1),
                    selectInput("model_type", "Model Type:",
                                choices = c("Random Forest" = "rf",
                                            "Linear Regression" = "lm",
                                            "Support Vector Machine" = "svmRadial"),
                                selected = "rf"),
                    sliderInput("train_split", "Training Data Split (%):",
                                min = 50, max = 90, value = 80, step = 5),
                    actionButton("train_model", "Train Model",
                                 class = "btn btn-success btn-block", 
                                 icon = icon("play"))
                ),
                box(title = "Model Results", width = 8, status = "info", solidHeader = TRUE,
                    tabsetPanel(
                      nav_panel("Model Performance",
                                h4("Model Metrics", class = "text-primary"),
                                DTOutput("model_metrics"),
                                hr(),
                                h4("Performance Plot", class = "text-primary"),
                                plotlyOutput("model_performance_plot")
                      ),
                      nav_panel("Predictions",
                                h4("Prediction Results", class = "text-primary"),
                                DTOutput("prediction_table")
                      ),
                      nav_panel("Future Forecast",
                                plotlyOutput("future_prediction_chart")
                      )
                    ))
                )),
                # Contact Tab
                nav_panel("Contact Us",
                          fluidRow(
                            box(width = 20,
                                title = " ..   Contact Us",
                                status = "primary",
                                solidHeader = TRUE,
                                h3("..   Thank you"),
                                p("..   . Thank you for visiting our website! We appreciate your support and hope you find the information valuable."),
                                p("..  .  For questions or support, please contact:"),
                                p(".. .  SANYAM JAIN"),
                                p(".. .  DIVYANSHU SINGH"),
                                p("...    sanyamjain8905@gmail.com")
                            )
                          )
                
              )
    )
    
    )
}
# UI Definition
ui <- fluidPage(
  tags$head(
    # Include Font Awesome for icons
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
  ),
  uiOutput("page")
)

# Server Logic
server <- function(input, output, session) {
  # Initialize user database on first run
  init_user_db()
  rv <- reactiveValues(data = NULL)
  # State to track current page
  page_state <- reactiveVal("login")
  # Render page based on state
  output$page <- renderUI({
    switch(page_state(),
           "login" = login_page(),
           "register" = registration_page(),
           "landing" = landing_page(),
           "main_app" = main_app_ui()
    )
  })
  # Login button handler
  observeEvent(input$login_btn, {
    # Validate login credentials
    if (validate_login(input$login_username, input$login_password)) {
      showNotification("Login Successful!", type = "message")
      page_state("landing")
    } else {
      showNotification("Invalid username or password", type = "error")
    }
  })
  
  # Show registration page
  observeEvent(input$show_register, {
    page_state("register")
  })
  
  # Show login page
  observeEvent(input$show_login, {
    page_state("login")
  })
  
  # Registration button handler
  observeEvent(input$register_btn, {
    # Validate registration inputs
    if (input$reg_password != input$reg_confirm_password) {
      showNotification("Passwords do not match", type = "error")
      return()
    }
    
    # Validate email format (basic check)
    email_pattern <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
    if (!grepl(email_pattern, input$reg_email)) {
      showNotification("Invalid email format", type = "error")
      return()
    }
    
    # Attempt to register user
    if (register_user(input$reg_username, input$reg_password, input$reg_email)) {
      showNotification("Registration Successful!", type = "message")
      page_state("login")
    } else {
      showNotification("Registration Failed. Username or email might already exist.", type = "error")
    }
  })
  
  # Start app button handler
  observeEvent(input$start_app, {
    page_state("main_app")
  })
  
  # Logout button handler
  observeEvent(input$logout_btn, {
    page_state("login")
  })
  
  # File upload and data loading
  observeEvent(input$load_data, {
    req(input$file)
    
    withProgress(message = 'Loading data...', value = 0, {
      tryCatch({
        if (input$file_type == "excel") {
          rv$data <- read_excel(input$file$datapath)
        } else {
          rv$data <- read.csv(input$file$datapath,
                              header = input$header,
                              sep = input$sep)
        }
        
        # Update UI elements that depend on the data
        updateSelectizeInput(session, "country",
                             choices = unique(rv$data$country),
                             selected = unique(rv$data$country)[1:5])
        
        # Update other input choices based on column names
        updateSelectInput(session, "sort_by",
                          choices = names(rv$data))
        
        updateSelectInput(session, "var_x",
                          choices = names(rv$data))
        
        updateSelectInput(session, "var_y",
                          choices = names(rv$data))
        
        updateSelectInput(session, "target_var",
                          choices = names(select_if(rv$data, is.numeric)))
        
        updateCheckboxGroupInput(session, "pred_vars",
                                 choices = names(select_if(rv$data, is.numeric)))
        
        # Update range inputs
        if ("year" %in% names(rv$data)) {
          updateSliderInput(session, "year",
                            min = min(rv$data$year, na.rm = TRUE),
                            max = max(rv$data$year, na.rm = TRUE),
                            value = c(min(rv$data$year, na.rm = TRUE),
                                      max(rv$data$year, na.rm = TRUE)))
        }
        
        if ("biofuel_consumption" %in% names(rv$data)) {
          updateSliderInput(session, "consumption_range",
                            min = min(rv$data$biofuel_consumption, na.rm = TRUE),
                            max = max(rv$data$biofuel_consumption, na.rm = TRUE),
                            value = c(min(rv$data$biofuel_consumption, na.rm = TRUE),
                                      max(rv$data$biofuel_consumption, na.rm = TRUE)))
        }
        
      }, error = function(e) {
        showNotification(
          paste("Error loading file:", e$message),
          type = "error"
        )
      })
    })
  })
  
  # Display upload status
  output$upload_status <- renderText({
    if (!is.null(rv$data)) {
      paste("Data loaded successfully:", nrow(rv$data), "rows,", 
            ncol(rv$data), "columns")
    }
  })
  
  # Preview uploaded data
  output$preview_data <- renderDT({
    req(rv$data)
    datatable(
      head(rv$data, 10),
      options = list(
        scrollX = TRUE,
        dom = 't',
        ordering = FALSE
      )
    )
  })
  
  # Filtered data reactive
  filtered_data <- reactive({
    req(rv$data)
    rv$data %>%
      filter(
        year >= input$year[1] & year <= input$year[2],
        country %in% input$country,
        biofuel_consumption >= input$consumption_range[1],
        biofuel_consumption <= input$consumption_range[2]
      ) %>%
      arrange(
        if (input$sort_order == "asc") across(all_of(input$sort_by))
        else desc(across(all_of(input$sort_by)))
      )
  })
  
  # Dashboard summary boxes
  output$total_countries <- renderValueBox({
    req(filtered_data())
    valueBox(
      length(unique(filtered_data()$country)),
      "Countries",
      icon = icon("globe"),
      color = "green"
    )
  })
  
  output$year_range <- renderValueBox({
    req(filtered_data())
    valueBox(
      paste(min(filtered_data()$year), "-", max(filtered_data()$year)),
      "Year Range",
      icon = icon("calendar"),
      color = "aqua"
    )
  })
  
  output$avg_consumption <- renderValueBox({
    req(filtered_data())
    valueBox(
      round(mean(filtered_data()$biofuel_consumption, na.rm = TRUE), 2),
      "Avg. Consumption",
      icon = icon("gas-pump"),
      color = "navy"
    )
  })
  
  output$total_records <- renderValueBox({
    req(filtered_data())
    valueBox(
      nrow(filtered_data()),
      "Total Records",
      icon = icon("database"),
      color = "maroon"
    )
  })
  # Update 3D plot variable choices when data is loaded
  observe({
    req(rv$data)
    numeric_cols <- names(select_if(rv$data, is.numeric))
    updateSelectInput(session, "x_var", choices = numeric_cols, 
                      selected = numeric_cols[1])
    updateSelectInput(session, "y_var", choices = numeric_cols, 
                      selected = numeric_cols[2])
    updateSelectInput(session, "z_var", choices = numeric_cols, 
                      selected = numeric_cols[3])
  })
  
  # Render 3D scatter plot
  output$scatter_3d <- renderPlotly({
    req(rv$data, input$x_var, input$y_var, input$z_var)
    
    plot_ly(data = rv$data,
            x = as.formula(paste0("~", input$x_var)),
            y = as.formula(paste0("~", input$y_var)),
            z = as.formula(paste0("~", input$z_var)),
            type = "scatter3d",
            mode = "markers",
            marker = list(
              size = input$point_size,
              opacity = input$opacity,
              color = ~country,
              colorscale = 'Viridis'
            ),
            hoverinfo = "text",
            text = ~paste(
              "Country:", country,
              "<br>", input$x_var, ":", get(input$x_var),
              "<br>", input$y_var, ":", get(input$y_var),
              "<br>", input$z_var, ":", get(input$z_var)
            )
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = input$x_var),
          yaxis = list(title = input$y_var),
          zaxis = list(title = input$z_var)
        ),
        title = "3D Energy Consumption Analysis",
        showlegend = TRUE
      )
  })
  # Reset filters
  observeEvent(input$reset, {
    updateSliderInput(session, "year",
                      value = c(min(data$year, na.rm = TRUE),
                                max(data$year, na.rm = TRUE)))
    updateSelectizeInput(session, "country",
                         selected = unique(data$country)[1:5])
    updateSliderInput(session, "consumption_range",
                      value = c(min(data$biofuel_consumption, na.rm = TRUE),
                                max(data$biofuel_consumption, na.rm = TRUE)))
  })
  
  # Render data table
  output$table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      extensions = 'Buttons',
      filter = 'top',
      selection = 'single'
    )
  })
  
  
  # Summary statistics
  output$summary_table <- renderTable({
    req(input$calc_stats)
    summary <- filtered_data() %>%
      summarise(across(where(is.numeric), list(Mean = ~ mean(., na.rm = TRUE), Max = ~ max(., na.rm = TRUE), Min = ~ min(., na.rm = TRUE))))
    summary
  })
  
  # Dynamic visualization
  output$dynamic_plot <- renderPlotly({
    req(input$var_x, input$var_y)
    plot_data <- filtered_data()
    
    p <- ggplot(plot_data, aes_string(x = input$var_x, y = input$var_y, color = "country")) +
      theme_minimal() +
      labs(x = input$var_x, y = input$var_y)
    
    # Apply color scheme
    if (input$color_scheme != "Default") {
      p <- p + scale_color_viridis_d(option = tolower(input$color_scheme))
    }
    
    # Apply log scale if selected
    if (input$log_scale) {
      p <- p + scale_y_log10()
    }
    
    # Apply chart type
    switch(input$chart_type,
           "Scatter Plot" = {
             p <- p + geom_point(alpha = 0.7)
             if (input$show_trend) {
               p <- p + geom_smooth(method = input$trend_method, se = FALSE)
             }
           },
           "Bar Chart" = {
             p <- p + geom_bar(stat = "identity", position = "dodge")
           },
           "Line Chart" = {
             p <- p + geom_line() + geom_point()
           },
           "Box Plot" = {
             p <- p + geom_boxplot()
           },
           "Heat Map" = {
             p <- ggplot(plot_data, aes_string(x = input$var_x, y = "country", fill = input$var_y)) +
               geom_tile() +
               scale_fill_viridis_c() +
               theme_minimal()
           }
    )
    
    ggplotly(p) %>%
      layout(
        margin = list(l = 50, r = 50, b = 50, t = 50),
        showlegend = TRUE
      )
  })
  # Add this code to the server function, before the closing bracket:
  
  # Model training and prediction
  model_results <- reactiveVal(NULL)
  
  # Train model when button is clicked
  observeEvent(input$train_model, {
    req(rv$data, input$target_var, input$pred_vars)
    
    # Prepare data
    model_data <- rv$data %>%
      select(all_of(c(input$target_var, input$pred_vars))) %>%
      na.omit()
    
    # Split data
    set.seed(123)
    train_index <- createDataPartition(model_data[[input$target_var]], 
                                       p = input$train_split/100, 
                                       list = FALSE)
    train_data <- model_data[train_index, ]
    test_data <- model_data[-train_index, ]
    
    # Train model
    withProgress(message = 'Training model...', value = 0, {
      tryCatch({
        if (input$model_type == "rf") {
          model <- randomForest(
            as.formula(paste(input$target_var, "~.")),
            data = train_data,
            importance = TRUE
          )
        } else if (input$model_type == "lm") {
          model <- lm(
            as.formula(paste(input$target_var, "~.")),
            data = train_data
          )
        } else if (input$model_type == "svmRadial") {
          model <- train(
            as.formula(paste(input$target_var, "~.")),
            data = train_data,
            method = "svmRadial",
            trControl = trainControl(method = "cv", number = 5)
          )
        }
        
        # Make predictions
        predictions <- predict(model, test_data)
        
        # Calculate metrics
        mse <- mean((test_data[[input$target_var]] - predictions)^2)
        rmse <- sqrt(mse)
        mae <- mean(abs(test_data[[input$target_var]] - predictions))
        r2 <- cor(test_data[[input$target_var]], predictions)^2
        
        # Store results
        model_results(list(
          model = model,
          predictions = predictions,
          actual = test_data[[input$target_var]],
          metrics = data.frame(
            Metric = c("MSE", "RMSE", "MAE", "R-squared"),
            Value = c(mse, rmse, mae, r2)
          )
        ))
        
      }, error = function(e) {
        showNotification(
          paste("Error training model:", e$message),
          type = "error"
        )
      })
    })
  })
  
  # Display model metrics
  output$model_metrics <- renderDT({
    req(model_results())
    datatable(
      model_results()$metrics,
      options = list(dom = 't')
    )
  })
  
  # Display performance plot
  output$model_performance_plot <- renderPlotly({
    req(model_results())
    
    plot_ly() %>%
      add_trace(
        x = model_results()$actual,
        y = model_results()$predictions,
        type = "scatter",
        mode = "markers",
        name = "Predictions",
        marker = list(color = "blue")
      ) %>%
      add_trace(
        x = range(model_results()$actual),
        y = range(model_results()$actual),
        type = "scatter",
        mode = "lines",
        name = "Perfect Prediction",
        line = list(color = "red", dash = "dash")
      ) %>%
      layout(
        title = "Actual vs Predicted Values",
        xaxis = list(title = "Actual Values"),
        yaxis = list(title = "Predicted Values")
      )
  })
  
  # Display prediction table
  output$prediction_table <- renderDT({
    req(model_results())
    prediction_df <- data.frame(
      Actual = model_results()$actual,
      Predicted = model_results()$predictions,
      Error = model_results()$predictions - model_results()$actual
    )
    datatable(
      prediction_df,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # Generate future predictions
  output$future_prediction_chart <- renderPlotly({
    req(model_results(), input$future_years)
    
    # Get the last few years of data
    last_years <- tail(sort(unique(rv$data$year)), 10)
    historical_values <- rv$data %>%
      filter(year %in% last_years) %>%
      group_by(year) %>%
      summarise(value = mean(!!sym(input$target_var), na.rm = TRUE))
    
    # Generate future years
    future_years <- seq(max(last_years) + 1, 
                        max(last_years) + input$future_years)
    
    # Create plot
    plot_ly() %>%
      add_trace(
        x = historical_values$year,
        y = historical_values$value,
        type = "scatter",
        mode = "lines+markers",
        name = "Historical",
        line = list(color = "blue")
      ) %>%
      add_trace(
        x = future_years,
        y = rep(tail(historical_values$value, 1), length(future_years)) * 
          (1 + rnorm(length(future_years), 0.02, 0.01)),
        type = "scatter",
        mode = "lines+markers",
        name = "Forecast",
        line = list(color = "red", dash = "dash")
      ) %>%
      layout(
        title = "Historical Values and Future Forecast",
        xaxis = list(title = "Year"),
        yaxis = list(title = input$target_var)
      )
  })
  
  # Reset filters
  observeEvent(input$reset, {
    updateSliderInput(session, "year",
                      value = c(min(rv$data$year), max(rv$data$year)))
    updateSelectizeInput(session, "country",
                         selected = unique(rv$data$country)[1:5])
    updateSliderInput(session, "consumption_range",
                      value = c(min(rv$data$biofuel_consumption),
                                max(rv$data$biofuel_consumption)))
    updateSelectInput(session, "sort_by",
                      selected = names(rv$data)[1])
    updateSelectInput(session, "sort_order",
                      selected = "asc")
  })
  
  # Main data table
  output$table <- renderDT({
    req(filtered_data())
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # Summary statistics
  observeEvent(input$calc_stats, {
    req(filtered_data())
    
    output$summary_table <- renderTable({
      filtered_data() %>%
        select_if(is.numeric) %>%
        summarise(
          across(everything(),
                 list(
                   Mean = ~mean(., na.rm = TRUE),
                   Median = ~median(., na.rm = TRUE),
                   SD = ~sd(., na.rm = TRUE),
                   Min = ~min(., na.rm = TRUE),
                   Max = ~max(., na.rm = TRUE)
                 ),
                 .names = "{.col}_{.fn}"
          )
        ) %>%
        tidyr::pivot_longer(everything(),
                            names_to = c("Variable", "Statistic"),
                            names_pattern = "(.*)_(.*)",
                            values_to = "Value") %>%
        tidyr::pivot_wider(names_from = Statistic,
                           values_from = Value)
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)



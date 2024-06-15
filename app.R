source("Libraries_Forecasting.R")
library(listviewer)

# Read the configuration file
config <- yaml::read_yaml("config.yaml")

# Access database credentials from the configuration
db_host <- config$database$host
db_name <- config$database$database
db_user <- config$database$user
db_password <- config$database$password

con <- dbConnect(MySQL(), 
                 dbname = db_name,
                 host = db_host,
                 user = db_user,
                 password = db_password)

# Function to fetch data from the database
fetch_data <- function(company) {
  base_query <- paste0("select Date, Close from Ratios_Tech.Trading_Data where Ticker = '", company, "'")
  data <- dbGetQuery(con, base_query)
  return(data)
}

unique_companies <- c("AAPL", "TSLA", "MSFT", "GOOGL")
library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { 
        background: url('Quant.jpeg') no-repeat center center fixed; 
        background-size: cover;
        color: #ffffff;
        font-family: 'Arial', sans-serif;
        min-height: 100vh;
        background-color: #121212; /* Fallback background color */
      }
      .title-bar {
        color: #ffffff; 
        background: linear-gradient(to right, #b8860b, #daa520);
        padding: 15px; 
        border-radius: 10px; 
        box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);
        text-align: center;
      }
      .sidebar {
        width: 350px; 
        background-color: rgba(31, 31, 31, 0.9); 
        padding: 20px;
        border-radius: 10px; 
        box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);
        margin-top: 20px;
      }
      .sidebar input, .sidebar select, .sidebar button {
        width: 100%;
        margin-bottom: 10px;
      }
      .sidebar button {
        background: linear-gradient(to right, #b8860b, #daa520);
        color: #ffffff;
        border: none;
        padding: 10px;
        border-radius: 5px;
        cursor: pointer;
        transition: transform 0.2s;
      }
      .sidebar button:hover {
        transform: scale(1.05);
      }
      .plot-border {
        border: 2px solid #b8860b;
        border-radius: 10px;
        box-shadow: 5px 5px 10px #888888;
        margin-top: 20px;
        background-color: rgba(31, 31, 31, 0.9);
        padding: 10px;
      }
    .table-style {
      font-size: 14px;
      color: #ffffff;
      border-collapse: collapse;
      width: 100%;
      border: 1px solid #2a2a2a;
      border-radius: 5px;
      margin-top: 20px;
      background-color: #1a1a1a;
    }

    .table-style th, .table-style td {
      border: 1px solid #2a2a2a;
      text-align: left;
      padding: 8px;
    }

    .table-style th {
      background-color: #daa520;
      color: #000000;
    }

    .table-style tr:nth-child(even) {
      background-color: #262626;
    }

    .table-style tr:nth-child(odd) {
      background-color: #1f1f1f;
    }

    .table-style tr:hover {
      background-color: #333333;
    }

    .performance-metrics {
      color: #b8860b;
      font-size: 20px;
      margin-bottom: 20px;
    }
      .metric-explanation {
        font-size: 14px;
        color: #b0b0b0;
      }
      .logo-container {
        text-align: center;
        margin-top: 20px;
        margin-bottom: 20px;
      }
      .logo-container img {
        max-height: 100px;
        width: auto;
      }
    "))
  ),
  titlePanel(
    div(class = "title-bar", "SafeGuard")
  ),
  sidebarLayout(
    sidebarPanel(
      style = "width: 350px; margin-top: 50px;",  # Adjusted width here
      class = "sidebar",
      selectInput("company1", "Select Company 1", choices = c("Apple Inc", "Alphabet Inc.", "Microsoft Corporation, Inc.", "Tesla, Inc."), selected = NULL),
      selectInput("company2", "Select Company 2", choices = c("Apple Inc", "Alphabet Inc.", "Microsoft Corporation, Inc.", "Tesla, Inc."), selected = NULL),
      selectInput("company3", "Select Company 3", choices = c("Apple Inc", "Alphabet Inc.", "Microsoft Corporation, Inc.", "Tesla, Inc."), selected = NULL),
      selectInput("company4", "Select Company 4", choices = c("Apple Inc", "Alphabet Inc.", "Microsoft Corporation, Inc.", "Tesla, Inc."), selected = NULL),
      actionButton("submit", "Submit"),
      div(class = "performance-metrics", strong("📊 Performance Metrics")),
      div(class = "table-style", tableOutput("metrics_table")),
      div(
        class = "metric-explanation",
        p(strong("MAPE:"), " Mean Absolute Percentage Error"),
        p(strong("MASE:"), " Mean Absolute Scaled Error"),
        p(strong("RMSE:"), " Root Mean Squared Error")
      )
    ),
    mainPanel(
      div(
        class = "logo-container",
        img(src = "logo.png", height = "100px")  # Adjust the height as needed
      ),
      fluidRow(
        column(6, div(plotlyOutput("plot1"), class = "plot-border")),
        column(6, div(plotlyOutput("plot2"), class = "plot-border"))
      ),
      fluidRow(
        column(6, div(plotlyOutput("plot3"), class = "plot-border")),
        column(6, div(plotlyOutput("plot4"), class = "plot-border"))
      )
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  observeEvent(input$submit, {
    # Define company names and corresponding tickers
    companies1 <- c(input$company1, input$company2, input$company3, input$company4)
    ticker_dict <- list("Apple Inc" = "AAPL", "Alphabet Inc." = "GOOGL", "Microsoft Corporation, Inc." = "MSFT", "Tesla, Inc." = "TSLA")
    companies <- sapply(companies1, function(company) ticker_dict[[company]])
    
    # Fetch data for each selected company
    data <- lapply(companies, fetch_data)
    
    # Prepare data by converting Date to Date type and Close to numeric
    data <- lapply(data, function(df) {
      df$Date <- as.Date(df$Date)
      df$Close <- as.double(df$Close)
      return(df)
    })
    
    # Model fitting and forecasting
    model_results <- lapply(data, function(df) {
      splits <- df %>% time_series_split(assess = "6 months", cumulative = TRUE)
      
      model_fit_prophet_boost <- prophet_boost() %>%
        set_engine("prophet_xgboost") %>%
        fit(Close ~ Date, data = training(splits))
      
      calibration_table <- model_fit_prophet_boost %>%
        modeltime_calibrate(testing(splits))
      
      if (is.null(calibration_table)) {
        list(forecast_results = NULL, accuracy_metric = NULL)
      } else {
        forecast_results <- calibration_table %>%
          modeltime_forecast(h = "6 months", actual_data = df)
        
        accuracy_data <- calibration_table %>%
          modeltime_accuracy() %>%
          table_modeltime_accuracy(.interactive = FALSE)
        
        accuracy_metric <- accuracy_data$`_data`
        
        list(forecast_results = forecast_results, accuracy_metric = accuracy_metric)
      }
    })
    
    # Check for null forecast tables
    null_tables <- sapply(model_results, function(result) is.null(result$forecast_results))
    if (any(null_tables)) {
      warning("Some calibration tables are null. Check model fitting and calibration.")
    }
    
    # Render plots for each company
    lapply(seq_along(companies), function(i) {
      local_i <- i  
      
      output[[paste0("plot", local_i)]] <- renderPlotly({
        if (!null_tables[local_i]) {
          plot_data <- plot_modeltime_forecast(
            model_results[[local_i]]$forecast_results,
            .interactive = TRUE,
            .facet_ncol = 1,
            .title = paste("Forecast Plot for", companies1[local_i]),
            .x_lab = "",
            .y_lab = ""
          )
          
          # Customize line colors
          plot_data <- plot_data %>%
            style(
              traces = c(1, 2, 3),  # Adjust indices based on your plot structure
              line = list(
                color = c('purple', 'green', 'orange')  # Customize colors
              )
            )
          
          # Update plot layout for better aesthetics
          plot_data <- plot_data %>%
            layout(
              title = list(
                text = paste("Forecast Plot for", companies1[local_i]),
                font = list(color = '#ffffff', size = 20, family = 'Arial')
              ),
              xaxis = list(title = "", titlefont = list(color = '#ffffff'), tickfont = list(color = '#ffffff')),
              yaxis = list(title = "", titlefont = list(color = '#ffffff'), tickfont = list(color = '#ffffff')),
              plot_bgcolor = 'rgba(0,0,0,0)',
              paper_bgcolor = 'rgba(0,0,0,0)',
              font = list(color = '#ffffff'),
              hovermode = 'closest',
              legend = list(x = 0.1, y = -0.1, font = list(color = '#ffffff'), orientation = "h"),  # Adjust legend position and orientation
              margin = list(l = 50, r = 50, b = 50, t = 50)
            )
          
          plot_data
        } else {
          plot_ly(type = 'scatter', mode = 'markers', x = numeric(0), y = numeric(0))
        }
      })
    })
    
    # Render metrics table
    output$metrics_table <- renderTable({
      accuracy_metrics <- lapply(seq_along(companies), function(i) {
        result <- model_results[[i]]
        accuracy_metric <- result$accuracy_metric
        accuracy_metric$Company <- companies1[i]
        return(accuracy_metric)
      })
      
      accuracy_metrics <- bind_rows(accuracy_metrics)
      
      accuracy_metrics <- accuracy_metrics %>%
        select(-.model_id, -.model_desc, -.type, -rsq) %>%
        rename(
          MAPE = mape,
          MASE = mase,
          RMSE = rmse,
          Company = Company
        ) %>%
        select(Company, MAPE, MASE, RMSE)
      
      accuracy_metrics
    }, striped = TRUE, bordered = TRUE, hover = TRUE, class = "table-style")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
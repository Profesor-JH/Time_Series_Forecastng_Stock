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
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;700&display=swap", 
      rel = "stylesheet"
    ),
    tags$style(HTML("
      body { 
        background: url('Quant_2.jpeg') no-repeat center center fixed; 
        background-size: cover;
        color: #ffffff;
        font-family: 'Arial', sans-serif;
        min-height: 100vh;
        background-color: #121212; /* Fallback background color */
      }
      .title-bar {
          position: relative;
          display: inline-block;
          color: #0000FF; /* Pure blue color */
          background: linear-gradient(to right, #87CEEB, #00BFFF); /* Sky blue gradient */
          width: 100%;
          text-align: center;
          font-size: 100px;
          font-weight: bold;
          margin-bottom: 20px; /* Added margin */
          padding: 15px 30px; /* Increased padding for better spacing */
          border-radius: 10px;
          box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);
          animation: title-glow 1.5s infinite alternate; /* Adding glow animation */
          font-family: 'Poppins', sans-serif; /* Professional font */
      }
      
      .title-bar .emoji-container {
        font-size: 100px; /* Increase emoji size */
        animation: emoji-bounce 1.5s infinite; /* Adding bounce animation to emoji */
        color: #0000FF; /* Ensuring emoji color is visible */
        margin-left: 10px; /* Adjust margin as needed */
      }
      
      @keyframes title-glow {
          from {
              text-shadow: 0 0 15px #87CEEB, 0 0 30px #00BFFF, 0 0 45px #87CEEB, 0 0 60px #00BFFF;
          }
          to {
              text-shadow: 0 0 30px #00BFFF, 0 0 45px #87CEEB, 0 0 60px #00BFFF, 0 0 75px #87CEEB;
          }
      }
      
      @keyframes emoji-bounce {
          0%, 20%, 50%, 80%, 100% {
              transform: translateY(0);
          }
          40% {
              transform: translateY(-20px);
          }
          60% {
              transform: translateY(-10px);
          }
      }


      .sidebar {
        width: 350px; 
        background-color: rgba(31, 31, 31, 0.9); 
        padding: 20px;
        border-radius: 10px; 
        box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);
        margin-bottom: 0px;
        font-size: 20px;
        margin-top: 10px; /* Adjusted margin top */
      }
      .sidebar input, .sidebar select, .sidebar button {
        width: 100%;
        margin-bottom: 10px;
        font-size: 20px;
        font-weight: bold;
        padding: 12px; /* Increased padding */
      }
      .sidebar button {
        background: linear-gradient(to right, #87CEEB, #00BFFF);
        color: #ffffff;
        border: none;
        padding: 12px;
        border-radius: 5px;
        cursor: pointer;
        transition: transform 0.2s;
        font-size: 18px;
        font-weight: bold;
      }
      .sidebar button:hover {
        transform: scale(1.05);
      }
      .plot-border {
        border: 5px solid #1E90FF; /* SkyBlue color */
        border-radius: 10px;
        box-shadow: 5px 5px 10px #888888;
        margin-top: 20px;
        background-color: rgba(255, 255, 255, 0.7); /* White transparent background */

        padding: 10px;
        margin-bottom: 20px; /* Added margin */
      }
      .plot-title {
          position: relative;
          display: inline-block;
          padding: 10px 20px; /* Padding for better visual separation */
          border-radius: 10px; /* Rounded corners */
          text-align: center;
          font-size: 30px; /* Increased font size for more impact */
          
          background: linear-gradient(to right, #87CEEB, #00BFFF); /* Gradient color */
          -webkit-background-clip: text;
          color: #ffffff; /* Makes the text transparent to show the gradient */
          text-shadow: 2px 2px 4px rgba(0,0,0,0.6);
          animation: title-glow 1.5s infinite alternate;
          margin-bottom: 20px; /* Increased margin bottom for spacing */
          padding: 10px; /* Added padding for better visual separation */
          border-radius: 10px; /* Rounded corners */
      }
      
      .plot-title::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          background-color: rgba(255, 255, 255, 0.7); /* White transparent background */
          border-radius: 10px; /* Rounded corners */
          z-index: -1; /* Place the background behind the text */
      }

      .plot-title.left-aligned {
        text-align: left;
      }
      
      .plot-title.right-aligned {
        text-align: right;
      }
      @keyframes title-glow {
        from {
          text-shadow: 2px 2px 4px rgba(0,0,0,0.6), 0 0 5px #87CEEB, 0 0 10px #87CEEB, 0 0 15px #87CEEB, 0 0 20px #87CEEB, 0 0 25px #87CEEB;
        }
        to {
          text-shadow: 2px 2px 4px rgba(0,0,0,0.6), 0 0 10px #00BFFF, 0 0 20px #00BFFF, 0 0 30px #00BFFF, 0 0 40px #00BFFF, 0 0 50px #00BFFF;
        }
      }

      .table-style {
        font-size: 18px;
        color: #ffffff;
        width: 100%;
        margin-top: 20px;
        margin-bottom: 20px; /* Added margin */
      }
      
      .table-style th, .table-style td {
        border: 1px solid #0000FF;
        padding: 10px; /* Increased padding */
      }
      
      .table-style th {
        background-color: #87CEEB; /* Blue header */
        color: #0000FF;
        font-weight: bold;
        
      }
      
      .table-style td:first-child {
        text-align: left; /* Left-align the first column */
      }
      
      .table-style td {
        text-align: center; /* Center-align other columns */
      }
      
      .table-style tr:nth-child(even) {
        background-color: #87CEEB;
        color: #000000; /* White text for even rows */
      }
      
      .table-style tr:nth-child(odd) {
        background-color: #1f1f1f;
        color: #000000; /* blue text for odd rows */
      }
      
      .table-style tr:hover {
        background-color: #333333;
      }
      
      .performance-metrics {
        color: #87CEEB; /* SkyBlue text */
        font-size: 20px; /* Increased font size */
        font-weight: bold; /* Added bold font weight */
        margin-bottom: 20px;
      }
      .metric-explanation {
        font-size: 16px; /* Increased font size */
        color: #b0b0b0;
      }
      .logo-container {
        text-align: center;
        margin-top: 20px;
        margin-bottom: 20px;
      }
      .logo-container img {
        max-height: 250px; /* Increased maximum height */
        width: auto;
      }
    "))
  ),
  titlePanel(
    div(class = "title-bar", HTML("S.a.f.e.G.u.a.r.d <span class='emoji'>👁️👁️</span>"))
  ),
  sidebarLayout(
    sidebarPanel(
      style = "width: 540px; margin-top: 50px;",  # Adjusted width here
      class = "sidebar",
      selectInput("company1", "Select Company 1", choices = c("Apple Inc", "Alphabet Inc.", "Microsoft Corporation, Inc.", "Tesla, Inc."), selected = NULL),
      selectInput("company2", "Select Company 2", choices = c("Apple Inc", "Alphabet Inc.", "Microsoft Corporation, Inc.", "Tesla, Inc."), selected = NULL),
      selectInput("company3", "Select Company 3", choices = c("Apple Inc", "Alphabet Inc.", "Microsoft Corporation, Inc.", "Tesla, Inc."), selected = NULL),
      selectInput("company4", "Select Company 4", choices = c("Apple Inc", "Alphabet Inc.", "Microsoft Corporation, Inc.", "Tesla, Inc."), selected = NULL),
      actionButton("submit", "Submit", style = "color: #0000FF;"),
      div(class = "performance-metrics", strong("📊 Performance Metrics")),
      div(class = "table-style", tableOutput("metrics_table")),
      div(
        class = "metric-explanation",
        p(strong("MAPE:"), " The more smaller the best"),
        p(strong("MASE:"), " The more smaller the best"),
        p(strong("RMSE:"), " The more smaller the best")
      )
    ),
    mainPanel(
      fluidRow(
        column(6, div(textOutput("title1"), class = "plot-title"), div(plotlyOutput("plot1"), class = "plot-border")),
        column(6, div(textOutput("title2"), class = "plot-title"), div(plotlyOutput("plot2"), class = "plot-border"))
      ),
      fluidRow(
        column(6, div(textOutput("title3"), class = "plot-title"), div(plotlyOutput("plot3"), class = "plot-border")),
        column(6, div(textOutput("title4"), class = "plot-title"), div(plotlyOutput("plot4"), class = "plot-border"))
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
          modeltime_refit(df) %>%
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
      
      output[[paste0("title", local_i)]] <- renderText({
        paste("Forecasts for", companies1[local_i])
      })
      
      
      
      output[[paste0("plot", local_i)]] <- renderPlotly({
        if (!null_tables[local_i]) {
          updated_forecast_results <- model_results[[local_i]]$forecast_results %>%
            mutate(.model_desc = ifelse(.model_desc == "PROPHET", "FORECAST", .model_desc))
          #print(unique(updated_forecast_results)$.conf_lo)
          plot_data <- plot_modeltime_forecast(
            updated_forecast_results,
            .interactive = TRUE,
            .conf_interval_show = TRUE,
            .conf_interval_fill = "white",
            #.plotly_slider = TRUE,
            .facet_ncol = 1,
            .title = FALSE,
            .legend_show = TRUE,
            .x_lab = "",
            .y_lab = ""
          )
          
          
          # Customize line colors and names
          plot_data <- plot_data %>%
            style(
              traces = c(1,2,3,4),  # Adjust indices based on your plot structure
              line = list(
                #color = c('gold', 'yellow', 'yellow', 'yellow'),  # Customize colors for each trace
                width = c(3.5, 3.5, 3.5, 3.5)  # Customize line widths if needed
              )
            )
          
          # Update plot layout for better aesthetics
          plot_data <- plot_data %>%
            layout(
              xaxis = list(
                title = "", 
                titlefont = list(color = '#000000', family = 'Arial', size = 14, bold = TRUE), 
                tickfont = list(color = '#000000', family = 'Arial', size = 10, bold = FALSE),
                tickformat = "%d-%b-%Y" # Format dates with more granularity
                #tickmode = "linear",      # Ensures even tick spacing
                #dtick = "M3",             # Shows every month
                #tickangle = 90
              ),
              yaxis = list(
                title = "", 
                titlefont = list(color = '#000000', family = 'Arial', size = 14, bold = TRUE), 
                tickfont = list(color = '#000000', family = 'Arial', size = 12, bold = TRUE)
              ),
              plot_bgcolor = 'rgba(0,0,0,0)',
              paper_bgcolor = 'rgba(0,0,0,0)',
              font = list(color = 'gold'),
              hovermode = 'closest',
              legend = list(x = 0.2, y = 1.2, font = list(color = 'blue'), orientation = "h"),  # Adjust legend position and orientation
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

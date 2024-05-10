library(shiny)
library(modeltime)
library(timetk)
library(tidymodels)
library(modeltime)
library(timetk)   
library(lubridate)
library(tidyverse)
library(DBI)
library(RMySQL)
library(tidyverse)  # For data manipulation
library(yaml)       # For reading YAML config file
library(dplyr)
library(rstan)
library(tidyr)
library(parsnip)
library(modeltime.gluonts)
library(tidymodels)
library(tidyverse)
library(plotly)


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


unique_companies<-c("Apple Inc","Alphabet Inc.","Microsoft Corporation, Inc.","Tesla, Inc.")


server <- function(input, output) {
  observeEvent(input$submit, {
    companies1 <- c(input$company1, input$company2, input$company3, input$company4)
    # Define the ticker dictionary
    ticker_dict <- list("Apple Inc" = "AAPL", "Alphabet Inc." = "GOOGL", "Microsoft Corporation, Inc." = "MSFT", "Tesla, Inc." = "TSLA")
    
    # Initialize an empty vector to store tickers
    companies <- c()

    # Update tickers vector with the tickers corresponding to selected companies
    companies <- sapply(companies1, function(company) ticker_dict[[company]])
    print(companies)  
    
    # Fetch data for selected companies
    data <- lapply(companies, fetch_data)
    print(data)
    
    # Preprocess data
    data <- lapply(data, function(df) {
      df$Date <- as.Date(df$Date)
      df$Close <- as.double(df$Close)
      return(df)
    })
    
    
    # Train models and generate forecasts
    forecasts <- lapply(data, function(df) {
      splits <- df %>% time_series_split(assess = "6 months", cumulative = TRUE)
      
      model <- switch(input$model,
                      "Auto ARIMA" = {
                        model_fit_arima <- arima_reg() %>%
                          set_engine("auto_arima") %>%
                          fit(Close ~ Date, training(splits))
                        
                        model_fit_arima
                      },
                      "Prophet" = {
                        model_fit_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
                          set_engine("prophet") %>%
                          fit(Close ~ Date, training(splits))
                        
                        model_fit_prophet
                      },
                      "Elastic Net" = {
                        recipe_spec <- recipe(Close ~ Date, data = training(splits)) %>%
                          step_timeseries_signature(Date) %>%
                          step_rm(contains("am.pm"), contains("hour"), contains("minute"),
                                  contains("second"), contains("xts")) %>%
                          step_fourier(Date, period = 365, K = 5) %>%
                          step_dummy(all_nominal())
                        
                        model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
                          set_engine("glmnet")
                        
                        workflow_fit_glmnet <- workflow() %>%
                          add_model(model_spec_glmnet) %>%
                          add_recipe(recipe_spec %>% step_rm(Date)) %>%
                          fit(training(splits))
                        
                        workflow_fit_glmnet
                      },
                      "Random Forest" = {
                        recipe_spec <- recipe(Close ~ Date, data = training(splits)) %>%
                          step_timeseries_signature(Date) %>%
                          step_rm(contains("am.pm"), contains("hour"), contains("minute"),
                                  contains("second"), contains("xts")) %>%
                          step_fourier(Date, period = 365, K = 5) %>%
                          step_dummy(all_nominal())
                        
                        model_spec_rf <- rand_forest(trees = 500, min_n = 50, mode = "regression") %>%
                          set_engine("randomForest")
                        
                        workflow_fit_rf <- workflow() %>%
                          add_model(model_spec_rf) %>%
                          add_recipe(recipe_spec %>% step_rm(Date)) %>%
                          fit(training(splits))
                        
                        workflow_fit_rf
                      },
                      "Prophet Boost" = {
                        recipe_spec <- recipe(Close ~ Date, data = training(splits)) %>%
                          step_timeseries_signature(Date) %>%
                          step_rm(contains("am.pm"), contains("hour"), contains("minute"),
                                  contains("second"), contains("xts")) %>%
                          step_fourier(Date, period = 365, K = 5) %>%
                          step_dummy(all_nominal())
                        
                        model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE) %>%
                          set_engine("prophet_xgboost") 
                        
                        workflow_fit_prophet_boost <- workflow() %>%
                          add_model(model_spec_prophet_boost) %>%
                          add_recipe(recipe_spec) %>%
                          fit(training(splits))
                        
                        workflow_fit_prophet_boost
                      }
      )
      
      calibration_table <- model %>%
        modeltime_calibrate(testing(splits))
      
      forecast_results <- calibration_table %>%
        modeltime_refit(df) %>%
        modeltime_forecast(h = "6 months", actual_data = df)
      
      return(forecast_results)
    })
    
    # Render the forecast plots
    lapply(seq_along(companies), function(i) {
      local_i <- i  # Capture the current value of i
      
      output[[paste0("plot", local_i)]] <- renderPlotly({
        plot_modeltime_forecast(
          forecasts[[local_i]],
          .interactive = TRUE,
          .facet_ncol = 1,
          .title = paste("Forecast Plot for", companies[local_i]),
          .x_lab = "",
          .y_lab = ""
        )
      })
    })
  })
}

########################## part 2 ############################

ui1 <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        .table-style th {
          background-color: #007bff; /* Navy blue background color for header cells */
          color: #ffffff; /* White text color for header cells */
          font-weight: bold; /* Bold font for header cells */
        }

        .table-style td, .table-style th {
          border: 1px solid #dddddd; /* Gray border for all cells */
          padding: 8px; /* Padding for all cells */
        }

        .table-style tr:nth-child(even) {
          background-color: #f2f2f2; /* Light gray background color for even rows */
        }

        .sidebar {
          margin-top: 50px; /* Adjust the margin-top value as needed */
        }

        .title-panel {
          background-color: #007bff; /* Navy blue background color */
          color: #ffffff; /* White text color */
          padding: 10px; /* Padding for the title panel */
          border-radius: 5px; /* Rounded corners */
          width: calc(100% - 1270px); /* Adjust width to match sidebar width */
          margin-right: 500px; /* Adjust margin to match sidebar width */
          margin-bottom: 20px; /* Add margin at the bottom for separation */
          font-weight: bold; /* Bold font for header cells */
        }
        "
      )
    )
  ),
  div(
    titlePanel(
      div("Stock Price Forecaster ☕️", class = "title-panel")
    ),
    sidebarLayout(
      sidebarPanel(
        style = "width: 500px;",
        selectInput("model", "Select Forecasting Model", 
                    choices = c("Auto ARIMA", "Prophet", "Elastic Net", "Random Forest", "Prophet Boost"),
                    selected = "Auto ARIMA"),
        selectInput("company1", "Select Company 1", choices = unique_companies, selected = NULL),
        selectInput("company2", "Select Company 2", choices = unique_companies, selected = NULL),
        selectInput("company3", "Select Company 3", choices = unique_companies, selected = NULL),
        selectInput("company4", "Select Company 4", choices = unique_companies, selected = NULL),
        actionButton("submit", "Submit"),
        tags$div(
          style = "color: #ff0000; font-size: 18px; margin-bottom: 10px; margin-top: 50px;font-weight: bold;", # Adjust color and font-size as needed
          "☕️ Performance Metrics on Testing Set "
        ),
        tags$div(
          tableOutput("metrics_table"),
          style = "border-collapse: collapse; width: 100%; margin-top: 10px; font-size: 16px;",
          class = "table-style"
        ),
        tags$p("MAPE: Mean Absolute Percentage Error"),
        tags$p("MASE: Mean Absolute Scaled Error"),
        tags$p("RMSE: Root Mean Squared Error")
      ),
      mainPanel(
        tags$style(
          HTML(
            "
            .plot-border {
              border: 2px solid #007bff;
              border-radius: 10px;
              box-shadow: 5px 5px 10px #888888;
            }
            "
          )
        ),
        tags$div(
          style = "color: #ff0000; font-size: 25px; margin-bottom: 2px; margin-left: 10px; font-weight: bold;", # Adjust color, font-size, and other styles as needed
          "☕️ Visualize Forecasting with Ease "
        ),
        tags$div(
          style = "color: #ff0000; font-size: 10px; margin-bottom: 2px; margin-left: 45px;", # Adjust color and font-size as needed
          "Choose your algorithm and companies from the left side and enjoy accurate forecasting"
        ),
        tabsetPanel(
          tabPanel("Plots",
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
    )
  )
)




server1 <- function(input, output) {
  observeEvent(input$submit, {
    companies1 <- c(input$company1, input$company2, input$company3, input$company4)
    # Define the ticker dictionary
    ticker_dict <- list("Apple Inc" = "AAPL", "Alphabet Inc." = "GOOGL", "Microsoft Corporation, Inc." = "MSFT", "Tesla, Inc." = "TSLA")
    
    # Initialize an empty vector to store tickers
    companies <- c()
    
    # Update tickers vector with the tickers corresponding to selected companies
    companies <- sapply(companies1, function(company) ticker_dict[[company]])
    print(companies)  
    
    # Fetch data for selected companies
    data <- lapply(companies, fetch_data)
    
    # Preprocess data
    data <- lapply(data, function(df) {
      df$Date <- as.Date(df$Date)
      df$Close <- as.double(df$Close)
      return(df)
    })
    
    # Define a function to train models, calibrate, forecast, and calculate accuracy
    model_function <- function(df) {
      splits <- df %>% time_series_split(assess = "6 months", cumulative = TRUE)
      
      model <- switch(input$model,
                      "Auto ARIMA" = {
                        model_fit_arima <- arima_reg() %>%
                          set_engine("auto_arima") %>%
                          fit(Close ~ Date, data = training(splits))
                        
                        calibration_table <- model_fit_arima %>%
                          modeltime_calibrate(testing(splits))
                        
                        forecast_results <- calibration_table %>%
                          modeltime_refit(df) %>%
                          modeltime_forecast(h = "6 months", actual_data = df)
                        
                        accuracy_data <- calibration_table %>%
                          modeltime_accuracy() %>%
                          table_modeltime_accuracy(.interactive = FALSE)
                        
                        accuracy_metric <- accuracy_data$`_data`
                        
                        list(forecast_results = forecast_results, accuracy_metric = accuracy_metric)
                      },
                      "Prophet" = {
                        model_fit_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
                          set_engine("prophet") %>%
                          fit(Close ~ Date, data = training(splits))
                        
                        calibration_table <- model_fit_prophet %>%
                          modeltime_calibrate(testing(splits))
                        
                        forecast_results <- calibration_table %>%
                          modeltime_refit(df) %>%
                          modeltime_forecast(h = "6 months", actual_data = df)
                        
                        accuracy_data <- calibration_table %>%
                          modeltime_accuracy() %>%
                          table_modeltime_accuracy(.interactive = FALSE)
                        
                        accuracy_metric <- accuracy_data$`_data`
                        
                        list(forecast_results = forecast_results, accuracy_metric = accuracy_metric)
                      },
                      "Elastic Net" = {
                        recipe_spec <- recipe(Close ~ Date, data = training(splits)) %>%
                          step_timeseries_signature(Date) %>%
                          step_rm(contains("am.pm"), contains("hour"), contains("minute"),
                                  contains("second"), contains("xts")) %>%
                          step_fourier(Date, period = 365, K = 5) %>%
                          step_dummy(all_nominal())
                        
                        model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
                          set_engine("glmnet")
                        
                        workflow_fit_glmnet <- workflow() %>%
                          add_model(model_spec_glmnet) %>%
                          add_recipe(recipe_spec %>% step_rm(Date)) %>%
                          fit(data = training(splits))
                        
                        calibration_table <- workflow_fit_glmnet %>%
                          modeltime_calibrate(testing(splits))
                        
                        forecast_results <- calibration_table %>%
                          modeltime_refit(df) %>%
                          modeltime_forecast(h = "6 months", actual_data = df)
                        
                        accuracy_data <- calibration_table %>%
                          modeltime_accuracy() %>%
                          table_modeltime_accuracy(.interactive = FALSE)
                        
                        accuracy_metric <- accuracy_data$`_data`
                        
                        list(forecast_results = forecast_results, accuracy_metric = accuracy_metric)
                      },
                      "Random Forest" = {
                        recipe_spec <- recipe(Close ~ Date, data = training(splits)) %>%
                          step_timeseries_signature(Date) %>%
                          step_rm(contains("am.pm"), contains("hour"), contains("minute"),
                                  contains("second"), contains("xts")) %>%
                          step_fourier(Date, period = 365, K = 5) %>%
                          step_dummy(all_nominal())
                        
                        model_spec_rf <- rand_forest(trees = 500, min_n = 50, mode = "regression") %>%
                          set_engine("randomForest")
                        
                        workflow_fit_rf <- workflow() %>%
                          add_model(model_spec_rf) %>%
                          add_recipe(recipe_spec %>% step_rm(Date)) %>%
                          fit(data = training(splits))
                        
                        calibration_table <- workflow_fit_rf %>%
                          modeltime_calibrate(testing(splits))
                        
                        forecast_results <- calibration_table %>%
                          modeltime_refit(df) %>%
                          modeltime_forecast(h = "6 months", actual_data = df)
                        
                        accuracy_data <- calibration_table %>%
                          modeltime_accuracy() %>%
                          table_modeltime_accuracy(.interactive = FALSE)
                        
                        accuracy_metric <- accuracy_data$`_data`
                        
                        list(forecast_results = forecast_results, accuracy_metric = accuracy_metric)
                      },
                      "Prophet Boost" = {
                        recipe_spec <- recipe(Close ~ Date, data = training(splits)) %>%
                          step_timeseries_signature(Date) %>%
                          step_rm(contains("am.pm"), contains("hour"), contains("minute"),
                                  contains("second"), contains("xts")) %>%
                          step_fourier(Date, period = 365, K = 5) %>%
                          step_dummy(all_nominal())
                        
                        model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE) %>%
                          set_engine("prophet_xgboost") 
                        
                        workflow_fit_prophet_boost <- workflow() %>%
                          add_model(model_spec_prophet_boost) %>%
                          add_recipe(recipe_spec) %>%
                          fit(data = training(splits))
                        
                        calibration_table <- workflow_fit_prophet_boost %>%
                          modeltime_calibrate(testing(splits))
                        
                        forecast_results <- calibration_table %>%
                          modeltime_refit(df) %>%
                          modeltime_forecast(h = "6 months", actual_data = df)
                        
                        accuracy_data <- calibration_table %>%
                          modeltime_accuracy() %>%
                          table_modeltime_accuracy(.interactive = FALSE)
                        
                        accuracy_metric <- accuracy_data$`_data`
                        
                        list(forecast_results = forecast_results, accuracy_metric = accuracy_metric)
                      }
      )
      
      return(model)
    }
    
    # Train models, calibrate, forecast, and calculate accuracy
    model_results <- lapply(data, model_function)
    
    # Render the forecast plots
    lapply(seq_along(companies), function(i) {
      local_i <- i  # Capture the current value of i
      
      output[[paste0("plot", local_i)]] <- renderPlotly({
        plot_modeltime_forecast(
          model_results[[local_i]]$forecast_results,
          .interactive = TRUE,
          .facet_ncol = 1,
          .title = paste("Forecast Plot for", companies1[local_i]),
          .x_lab = "",
          .y_lab = ""
        )
      })
    })
    
    # Render the accuracy metrics table
    output$metrics_table <- renderTable({
      # Combine accuracy metrics for all companies
      accuracy_metrics <- lapply(seq_along(companies), function(i) {
        result <- model_results[[i]]
        accuracy_metric <- result$accuracy_metric
        accuracy_metric$Company <- companies1[i]
        return(accuracy_metric)
      })
      
      # Combine accuracy metrics for all companies into a single data frame
      accuracy_metrics <- dplyr::bind_rows(accuracy_metrics)
      
      # Remove the specified columns
      accuracy_metrics <- accuracy_metrics %>%
        select(-.model_id, -.model_desc, -.type, -rsq) %>%
        rename(
          MAPE = mape,
          MASE = mase,
          RMSE = rmse,
          Company = Company
        )
      
      # Arrange the columns in the desired order
      accuracy_metrics <- accuracy_metrics %>%
        select(Company, MAPE, MASE, RMSE)
      
      # Return accuracy metrics table
      accuracy_metrics
    })
  })
}



# Run the application
shinyApp(ui = ui1, server = server1)

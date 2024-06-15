library(shiny)
library(plotly)
library(tidymodels)
library(timetk)
library(modeltime)

# Define function to fetch data (you need to define your own fetch_data function)
fetch_data <- function(company_ticker) {
  # Your data fetching logic here
}

# Define UI
ui <- fluidPage(
  titlePanel("Stock Price Forecasting"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Select Forecasting Model", 
                  choices = c("Auto ARIMA", "Prophet", "Elastic Net", "Random Forest", "Prophet Boost"),
                  selected = "Auto ARIMA"),
      selectInput("company1", "Select Company 1", choices = unique_companies, selected = NULL),
      selectInput("company2", "Select Company 2", choices = unique_companies, selected = NULL),
      selectInput("company3", "Select Company 3", choices = unique_companies, selected = NULL),
      selectInput("company4", "Select Company 4", choices = unique_companies, selected = NULL),
      actionButton("submit", "Submit"),
      tableOutput("metrics_table")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 fluidRow(
                   column(6, plotlyOutput("plot1")),
                   column(6, plotlyOutput("plot2"))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("plot3")),
                   column(6, plotlyOutput("plot4"))
                 )
        )
      )
    )
  )
)

# Define server logic
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
                          modeltime_refit(data) %>%
                          modeltime_forecast(actual_data = data, h = "6 months")
                        
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
                          modeltime_refit(data) %>%
                          modeltime_forecast(actual_data = data, h = "6 months")
                        
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
                          modeltime_refit(data) %>%
                          modeltime_forecast(actual_data = data, h = "6 months")
                        
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
                          modeltime_refit(data) %>%
                          modeltime_forecast(actual_data = data, h = "6 months")
                        
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
                          modeltime_refit(data) %>%
                          modeltime_forecast(actual_data = data, h = "6 months")
                        
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
        ) %>%
        select(Company, MAPE, MASE, RMSE)
      
      # Return accuracy metrics table
      accuracy_metrics
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

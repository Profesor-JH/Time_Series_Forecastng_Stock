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


unique_companies<-c("AAPL","TSLA","MSFT","GOOGL")

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
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3"),
      plotOutput("plot4")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$submit, {
    companies <- c(input$company1, input$company2, input$company3, input$company4)
    
    # Fetch data for selected companies
    data <- lapply(companies, fetch_data)
    # Close the database connection when done
    dbDisconnect(con)
    # Preprocess data
    data <- lapply(data, function(df) {
      df$Date <- as.Date(df$Date)
      df$Close <- as.double(df$Close)
      return(df)
    })
    
    # Train models
    models <- lapply(data, function(df) {
      splits <- df %>% time_series_split(assess = "6 months", cumulative = TRUE)
      
      model <- switch(input$model,
                      "Auto ARIMA" = arima_reg() %>%
                        set_engine("auto_arima") %>%
                        fit(Close ~ Date, training(splits)),
                      "Prophet" = prophet_reg(seasonality_yearly = TRUE) %>%
                        set_engine("prophet") %>%
                        fit(Close ~ Date, training(splits)),
                      "Elastic Net" = {
                        recipe_spec <- recipe(Close ~ Date, training(splits)) %>%
                          step_timeseries_signature(Date) %>%
                          step_rm(contains("am.pm"), contains("hour"), contains("minute"),
                                  contains("second"), contains("xts")) %>%
                          step_fourier(Date, period = 365, K = 5) %>%
                          step_dummy(all_nominal())
                        
                        recipe_spec %>% prep() %>% juice()
                        
                        model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
                          set_engine("glmnet")
                        
                        workflow_fit_glmnet <- workflow() %>%
                          add_model(model_spec_glmnet) %>%
                          add_recipe(recipe_spec %>% step_rm(Date)) %>%
                          fit(training(splits))
                        
                        workflow_fit_glmnet
                      },
                      "Random Forest" = {
                        recipe_spec <- recipe(Close ~ Date, training(splits)) %>%
                          step_timeseries_signature(Date) %>%
                          step_rm(contains("am.pm"), contains("hour"), contains("minute"),
                                  contains("second"), contains("xts")) %>%
                          step_fourier(Date, period = 365, K = 5) %>%
                          step_dummy(all_nominal())
                        
                        recipe_spec %>% prep() %>% juice()
                        
                        model_spec_rf <- rand_forest(trees = 500, min_n = 50, mode = "regression") %>%
                          set_engine("randomForest")
                        
                        workflow_fit_rf <- workflow() %>%
                          add_model(model_spec_rf) %>%
                          add_recipe(recipe_spec %>% step_rm(Date)) %>%
                          fit(training(splits))
                        
                        workflow_fit_rf
                      },
                      "Prophet Boost" = {
                        model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE) %>%
                          set_engine("prophet_xgboost") 
                        
                        workflow_fit_prophet_boost <- workflow() %>%
                          add_model(model_spec_prophet_boost) %>%
                          add_recipe(recipe_spec) %>%
                          fit(training(splits))
                        
                        workflow_fit_prophet_boost
                      }
      )
      
      return(model)
    })

    # Forecast
    forecasts <- lapply(1:length(models), function(i) {
      calibration_table <- model_table %>%
        modeltime_calibrate(testing(splits))
      
      forecast_results <- calibration_table %>%
        modeltime_refit(data[[i]]) %>%
        modeltime_forecast(h = "6 months", actual_data = data[[i]])
      
      return(forecast_results)
    })
    print(data.frame(forecasts))
    # Render the forecast plots
    lapply(1:length(companies), function(i) {
      
      output[[paste0("plot",i)]] <- renderPlot({
        print("we are inside the renderplot")
        plot_modeltime_forecast(
          forecasts[[i]],
          .interactive = TRUE,
          .facet_ncol = 1,
          .title = paste("Forecast Plot for", companies[i]),
          .x_lab = "",
          .y_lab = ""
        )
      })
    })
  })
}



# Run the application
shinyApp(ui = ui, server = server)

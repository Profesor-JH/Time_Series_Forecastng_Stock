# Time Series Forecasting for Stock Prices

# Overview
This project uses time series analysis techniques to forecast stock prices. The forecasting models can predict future stock prices based on historical data. The project includes an interactive Shiny app for visualizing and comparing the forecasted prices for different companies.

**watch a demo here :** https://www.youtube.com/watch?v=cWBs3vQQGeQ

# Table of Contents

[Overview](Overview)
[Features](Features)
[Installation](Installation)
[Usage](Usage)
[Project Structure](Project Structure)
[Key Technologies](Key Technologies)
[Results](Results)
[Contributing](Contributing)
[License](License)
[Acknowledgements](Acknowledgements)



# Features
  **1. Data Collection:** Fetches historical stock price data from a database.
  **2. Data Preprocessing:** Cleans and prepares data for analysis.
  **3. Time Series Forecasting:** Implements multiple forecasting models including Auto ARIMA, Prophet, Elastic Net, Random Forest, and Prophet Boost.
  **4. Interactive Visualization:** Shiny app for visualizing forecast results.
  **5. Model Comparison:** Allows comparing forecasts from different models and companies.
Installation


# Prerequisites
* R (version 4.0 or above)
* RStudio (optional, but recommended)
* Required R packages (listed in Libraries_Forecasting.R)
* MySQL database with the relevant stock data

# Steps
## 1. Clone the repository:
```
git clone https://github.com/Profesor-JH/Time_Series_Forecastng_Stock.git
cd Time_Series_Forecasting_Stock
```
## 2. Install the required packages:

Open R or RStudio and run the following command:
```
source('Libraries_Forecasting.R')
```

## 3. Set up the configuration:
Update the `config.yaml` file with your database credentials and other necessary settings.

# Usage

## Running the Shiny App
To run the interactive Shiny app, execute the following command in your R environment:

```
source('Forecasting-App.R')
```

# Shiny App Details

**1. Select Forecasting Model:** Choose from Auto ARIMA, Prophet, Elastic Net, Random Forest, or Prophet Boost.

**2. Select Companies:** Choose up to four companies from the dropdown menus.

**3. Submit:** Click the "Submit" button to fetch data, train the models, and display the forecast plots.


# Project Structure
```
Time_Series_Forecasting_Stock/
├── Forecaster.R.              #The main script for forecasting stock prices.
├── Forecasting-App.R          #Shiny app for interactive forecasting.
├── Libraries_Forecasting.R.   #Script to install and load required libraries.
├── config.yaml                #Configuration file for setting parameters and options.
├── README.md                  #Project documentation.
└── Time_Series_Forecasting_Stock.Rproj #R project file.
```


# Key Technologies
* R Programming Language: Main language used for data analysis and forecasting.
* Shiny: Framework for building interactive web applications in R.
* ggplot2: Visualization library for creating plots.
* forecast: Package for time series forecasting.
* dplyr: Data manipulation package.
* yaml: Package for reading configuration files.

# Contributing
Contributions are welcome! Please follow these steps:

1. Fork the repository.
2. Create a new branch (git checkout -b feature/your-feature-name).
3. Make your changes.
4. Commit your changes (git commit -m 'Add some feature').
5. Push to the branch (git push origin feature/your-feature-name).
6. Open a pull request.

# License
This project is licensed under the MIT License - see the LICENSE file for details.

# Acknowledgements
Jean Henock Viayinon - Project Maintainer

# Other Contributors
Special thanks to the R community for their valuable libraries and tools.


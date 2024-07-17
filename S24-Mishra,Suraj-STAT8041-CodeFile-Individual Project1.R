## Import the libraries 
library(fpp3)       # For time series analysis
library(tidyverse)  # For data manipulation and visualization
library(knitr)      # For generating reports from R scripts
library(seasonal)   # For seasonal decomposition
library(lubridate)  # For date-time manipulation
library(ggplot2)    # For advanced plotting capabilities

## Import the dataset
tesla <- read.csv("D:/Conestoga/Predictive Analytics/Semester 2/STAT8041-Statistical Forecasting/Project 1/TSLA.csv")

##checking for any null values 
sum(is.na(tesla))

##checking any duplicate values

sum(duplicated(tesla))

# Define the exchange rate (example: 1 USD = 75 INR)
exchange_rate = 75

# Convert 'Close' prices from INR to USD
tesla$Close_USD <- tesla$Close / exchange_rate

# Select 'Date' and 'Close_USD' columns for time series analysis
tesla_stock <- tesla %>% select(Date, Close_USD)
tesla_stock

# Display the first few rows and structure of the dataset
head(tesla_stock)
str(tesla_stock)

# Convert 'Date' column to Date type
tesla_stock$Date <- as.Date(tesla_stock$Date)
tesla_stock
str(tesla_stock)



# Convert to tsibble object with 'Date' as index
tesla_stock1 <- tesla_stock %>% as_tsibble(index = Date)
tesla_stock1

# Update tsibble with row numbers as index
tesla_stock2 <- tesla_stock1 %>%
  rowid_to_column() %>%
  update_tsibble(index = rowid, regular = TRUE)

tesla_stock2

# Plot the Tesla stock before transformation close price
tesla_stock2 %>% autoplot(Close_USD) +
  labs(y = "Stock Close Price in USD",
       x = "Row ID",
       title = "Tesla Stock Close Price in USD")

# Plot autocorrelation of Tesla stock closing price before transformation
tesla_stock2 %>% ACF(Close_USD, lag_max = 48) %>%
  autoplot() +
  labs(title = "Autocorrelation plot of Tesla Stock Closing Price Before Tranformation" )

## Transformation 
lambda<- tesla_stock2 %>%
  features(Close_USD,features = guerrero) %>%
  pull(lambda_guerrero)

lambda

tesla_stock2 %>%
  autoplot(box_cox(Close_USD,lambda)) +
  labs(title="Box Cox transformer for stock closing prices")

tesla_stock2 %>% ACF(Close_USD, lag_max = 48) %>%
  autoplot() +
  labs(title = "Autocorrelation plot of Tesla Stock Closing Price after Tranformation" )

## Decomposition 
# Classical decomposition
dcmp_tesla_classical <- tesla_stock2 %>%
  model(
    classical = classical_decomposition(Close_USD ~ season(36), type = "additive"))

# Display components of classical decomposition
print(components(dcmp_tesla_classical))

# Extract classical components
classical_tesla <- dcmp_tesla_classical %>%
  select(classical) %>%
  components(dcmp_tesla_classical)

classical_tesla %>% tail() %>% kable()


# Plot classical components
classical_tesla %>% autoplot()

# STL decomposition
dcmp_tesla_stl <- tesla_stock2 %>%
  model(
    stl = STL(Close_USD)
  )

# Extract components from STL decomposition
stlcomp_tesla <- dcmp_tesla_stl %>% select(stl) %>% components(dcmp_tesla_stl)

stlcomp_tesla %>% tail() %>% kable()

# Plot STL components
stlcomp_tesla %>% autoplot()

## Train and test
# Filter data for training and testing sets
tesla_train <- tesla_stock2 %>% filter(year(Date) == 2021)
tesla_test <- tesla_stock2 %>% filter(year(Date) == 2022)

tesla_train
tesla_test

### Fit the models
# Fit various forecasting models
tesla_fit <- tesla_train %>%
  model(
    mean = MEAN(Close_USD),
    NaiveM = NAIVE(Close_USD),
    NaiveD = NAIVE(Close_USD ~ drift())
  )

# Forecast using the fitted models
tesla_fc <- tesla_fit %>% forecast(h = 150)

# Plot forecasts against test data
tesla_fc %>% autoplot(tesla_test, level = NULL)

# Evaluate forecast accuracy
accuracy(tesla_fc, tesla_test) %>% select(.model, RMSE, MAE)



# Plot residuals of Tesla model
# Plot residuals of NaiveM model which is bestout of other two models
naive_fit <- tesla_fit |> select(NaiveM)
naive_fit |> gg_tsresiduals()

## Autoplot with autolayer for tesla forecast
tesla_stock2 %>%
  autoplot(Close_USD)+
  autolayer(tesla_fc)










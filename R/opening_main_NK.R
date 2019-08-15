library(tidyverse)
library(lubridate)
library(glmnet)
library(caret)
library(Metrics)
library(SDMTools)
library(hydroGOF)
library(forecast)

install.packages('forecast')

## DATA

set_meta <- function(path, df) {
  nm <-
    strsplit(path, "/")[[1]] %>%
    tail(1) %>%
    strsplit('[.]') %>%
    unlist()

  add_column(
    Market = nm[2],
    Name = nm[1],
    df
  )
}

# equity data loader from source
# [...]

# equity data loader from csv files
dax_equities <- list.files(
  # change your relative path here :
  "C:/Users/alanaka/R/GoIT/tradeopening/data",
  full.names = TRUE,
  pattern = "\\.csv$"
)  %>%
  map(~ set_meta(.x, read_csv(
    .x,
    col_types = cols(
      Date = col_date(),
      .default = col_double()
    )
  ))
)

# data cleaner
dax_equities <- dax_equities %>%
  map(~ filter(.x, Open != "null")) %>%
  map(~ arrange(.x, Date)) %>%
  map(~ rename(.x, "AdjClose" = 'Adj Close')) %>%
  discard(~ nrow(.x) < 200)


## MODELL

# performance calculation and log trafos for raw data
get_performance <- function(df) {
  open <- pull(df, Open)
  close <- pull(df, Close)
  yday_open <- pull(df, Open) %>% lag
  yday_close <- pull(df, Close) %>% lag
  yday_low <- pull(df, Low) %>% lag
  yday_high <- pull(df, High) %>% lag

  df %>%
    add_column(
      OpeningPerf = log(open / yday_close),
      IntradayPerf = log(close / open),
      NormLow = log(yday_low / yday_open),
      NormHigh = log(yday_high / yday_open)
    )
}

# add additional time series / indicators (MAs for now)
get_indicators <- function(df) {
  yday_close <- pull(df, Close) %>% lag
  sma50 <- TTR::SMA(yday_close, n = 50)
  sma200 <- TTR::SMA(yday_close, n = 200)

  df %>%
    add_column(
      SMA50 = sma50,
      SMA200 = sma200,
      NormSMA50 = log(yday_close / sma50),
      NormSMA200 = log(yday_close / sma200)
    )
}


# model training and testing
get_training_data <- function(df) {
  df %>%
    select(
      OpeningPerf,
      IntradayPerf,
      NormLow,
      NormHigh,
      NormSMA50,
      NormSMA200
    ) %>%
    tail(-200)
}


prepare_data <- function(df) {
  df %>%
    map(get_performance) %>%
    map(get_indicators) %>%
    map(get_training_data) %>%
    map(as.matrix)
}


train <- function(data) {
  # lasso regression
  glmnet::cv.glmnet(
    data[,-6], data[, 6]
  )
}

get_result <- function(model, test_data) {
  tibble(
    prediction = c(predict(
      model, test_data[,-6],  s = "lambda.min"
    )),
    actual = test_data[,6]
  ) %>%
    mutate(errsq = (actual - prediction)^2)
}

## SIMULATION

# train set : DAX data until 2016
train_data <-
  dax_equities %>%
  map(~ filter(.x, year(Date) <= 2016)) %>%
  map(get_performance) %>%
  map(get_indicators) %>%
  map(get_training_data) %>%
  reduce(bind_rows) %>%
  as.matrix()

###------

train_data <-
  dax_equities %>%
  map(~ filter(.x, year(Date) <= 2016))

train_data[[1]]

train_data <- train_data  %>%
  map(get_performance)

train_data <- train_data %>%
  map(get_indicators)


glimpse(train_data[[1]])

glimpse(tail(train_data[[1]]))


train_data <- train_data %>%
  map(get_training_data)

train_data <- train_data %>%
reduce(bind_rows) %>%
  as.matrix()

train_data <- train_data %>%
  as.matrix()


glimpse(train_data)


glimpse(backtesting_data[[1]])

glimpse(returns[[1]])




##------------------



# test data : DAX data from 2017
test_data <-
  dax_equities %>%
  map(~ filter(.x, year(Date) >= 2017)) %>%
  prepare_data

model <-
  train_data %>%
  train

backtesting_data <-
  test_data %>%
  map(~ cbind(.x, get_result(model, .x)$prediction)) %>%
  map(as_tibble) %>%
  map(~ .x %>% rename(PredictedIntradayPerf = 7))


get_returns <- function(backtesting_data) {
  tibble(
    day = 1:nrow(backtesting_data),
    follow_signal = cumsum(
      sign(
        backtesting_data$PredictedIntradayPerf
      ) * backtesting_data$IntradayPerf
    ),
    buy_at_open_sell_at_close = cumsum(
      backtesting_data$IntradayPerf
    ),
    buy_and_hold = cumsum(
      backtesting_data$Close %>% TTR::ROC() %>% coalesce(., 0)
    )
  )
}


returns <-
  backtesting_data %>%
  map2(
    dax_equities, # append original raw data
    ~ cbind(.x, tail(.y, nrow(.x)))) %>%
  map(get_returns)


plots <-
 returns %>%
     map( ~
     .x %>%
       ggplot(aes(x = day)) +
       geom_line(aes(y = follow_signal, color = 'trade according to signal')) +
       geom_line(aes(y = buy_and_hold, color = 'buy&hold')) +
       geom_line(aes(y = buy_at_open_sell_at_close, color = 'buy open sell close')) +
       scale_color_manual(values = c(
         'trade according to signal' = 'cyan',
         'buy&hold' = 'darkblue',
         'buy open sell close' = 'green'
       )) +
       labs(color = 'strategy') +
       theme_bw() +
       ylab('return')
   )

titles <- dax_equities %>%
 map(~ pull(.x, Name) %>% head(1)) %>%
 unlist()

ggpubr::ggarrange(  # takes a while to render
 plotlist = plots,
 labels = titles,
 common.legend = TRUE,
 legend = 'bottom',
 hjust = -1.5
)


##---------------

summary(model)

backtesting_data_1 %>% mutate(  dif=
abs(backtesting_data_1$IntradayPerf - backtesting_data_1$PredictedIntradayPerf )
)


#### Lasso Regression Model Validation
#RMSE - Root Mean Square Error
#the RMSE also then squares the difference, finds the average of all the squares and then finds the square root.
#Sqrt(Avg(Power(Actual -Forecast)))
rmse(backtesting_data_1$IntradayPerf,  backtesting_data_1$PredictedIntradayPerf )
0.134086

#MAE - Mean Absolute Error
#It takes the absolute difference between the actual and forecasted values and finds the average.
#Avg(Abs(Actual—Forecast))
mae(backtesting_data_1$IntradayPerf,  backtesting_data_1$PredictedIntradayPerf )
0.1035268

#MAPE - Mean Absolute Percentage Error
#the mean absolute percentage error is good method in the sense that it is the percentage of the error
#compared to the actual value.
#Avg(Abs(Actual-Forecast)/Abs(Actual)) *100

d <- abs(backtesting_data_1$IntradayPerf-backtesting_data_1$PredictedIntradayPerf)/abs(backtesting_data_1$IntradayPerf)

mean(d[!is.infinite(d)])*100

mape( backtesting_data_1$PredictedIntradayPerf ,backtesting_data_1$IntradayPerf)
1.163362

#MinMax accuracy
mean(min(backtesting_data_1$IntradayPerf,  backtesting_data_1$PredictedIntradayPerf)/max(backtesting_data_1$IntradayPerf,  backtesting_data_1$PredictedIntradayPerf))
-2.407993

#NRMSE
nrmse( backtesting_data_1$PredictedIntradayPerf, backtesting_data_1$IntradayPerf )
703.8

# from forecast library, calulates the measures for the model accuracy
accuracy(backtesting_data_1$PredictedIntradayPerf, backtesting_data_1$IntradayPerf)

 mean(backtesting_data_1$IntradayPerf)
-0.002556021

mean(backtesting_data_1$PredictedIntradayPerf )
-0.03181983


#Thoughts:
#why do we perform CumSum to find the strategies?

#more feasible and interpretative if we predict only the Direction(up/down),
#so it is turned into a Classification problem, binomail

#Add Model validation
#how accurate is the prediction of the variable PredictedIntradayPerf

#compare PredictedIntradayPerf VS IntradayPerf

# Try Linear Discriminant Analysis model (LDA) at MASS library

preds <- backtesting_data_1$PredictedIntradayPerf
actual <- backtesting_data_1$IntradayPerf
rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
  -48.64154

#[1] 0.25

confusion matrix, accuracy and AUROC, …),

volatility, performance of the last day?



accuracy {SDMTools}

accuracy(backtesting_data_1$IntradayPerf, backtesting_data_1$PredictedIntradayPerf, threshold = 0.5)

calc_RMSE <- function(m, o){
  sqrt(mean((m - o)^2))
}

calc_RMSE( backtesting_data_1$PredictedIntradayPerf, backtesting_data_1$IntradayPerf) #0.134086

m is for model (fitted) values, o is for observed (true) values.

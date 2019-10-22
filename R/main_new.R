library(tidyverse)
library(lubridate)
library(furrr)
library(entropy)
library(dbscan)
library(xgboost)

plan(multiprocess)

source("R/backtest.R")
source("R/data.R")
source("R/train_lasso.R")
source("R/train_xgboost.R")



dax_equities <- list.files(
  "./data/",
  full.names = TRUE,
  pattern = "\\.csv$"
) %>%
  map(~ set_meta(.x, read_csv(
    .x,
    col_types = cols(
      Date = col_date(),
      .default = col_double()
    )
  ))) %>%
  map(~ filter(.x, Open != "null")) %>%
  map(~ arrange(.x, Date)) %>%
  map(~ rename(.x, "AdjClose" = `Adj Close`)) %>%
  map(~ tail(.x, -10)) %>%
  discard(~ nrow(.x) < 200)


dax_titles <-
  dax_equities %>%
  map(~ pull(.x, Name) %>% head(1)) %>%
  unlist()


train_data <-
  dax_equities %>%
  map(~ filter(.x, Date < "2018-01-01")) %>%
  prepare_data() %>%
  reduce(rbind)

test_data <- # validated against individual equities
  dax_equities %>%
  map(~ filter(.x, Date >= "2018-01-01")) %>%
  prepare_data()


norm_recency <- train_data$Recency %>% normalize01()

normalize01 <- function(x)
  (x - min(x)) / (max(x) - min(x))


lasso_model <-
  train_data %>%
  sample_frac( # 2/3 train samples w/o replacement
    size = 2 / 3,
    replace = F,
    weight = normalize01(train_data$Recency)
  ) %>%
  train_lasso()

xgboost_model <-
  train_data %>%
  sample_frac( # 2/3 train samples w/o replacement
    size = 2 / 3,
    replace = F,
    weight = normalize01(train_data$Recency)
  ) %>%
  train_xgb()


returns <-
  test_data %>%
  get_backtesting_data(lasso_model, xgboost_model, dax_equities) %>%
  map(get_returns)

returns %>% plot_return_densities()
returns %>% plot_equity()
returns %>% plot_separate_performances(dax_titles)

source("data.R")
source("train_lasso.R")
source("train_xgboost.R")

get_divergence <- function(data, lasso_weights, xgb_weights) {
  stopifnot(
    length(lasso_weights) == 5 &&
    length(xgb_weights) == 5
  )

  lasso_model <-
    data %>%
    sample_frac( # 2/3 train samples w/o replacement
      size = 2 / 3,
      replace = F,
      weight = normalize01(data$Recency)
    ) %>%
    train_lasso(lasso_weights)


  xgboost_model <-
    data %>%
    sample_frac( # 2/3 train samples w/o replacement
      size = 2 / 3,
      replace = F,
      weight = normalize01(data$Recency)
    ) %>%
    train_xgb(xgb_weights)

  div <- data %>% get_prediction(xgboost_model, lasso_model)

  tibble(
    LassoVolume              = lasso_weights[[1]],
    LassoCloseEntropy        = lasso_weights[[2]],
    LassoClosetoCloseEntropy = lasso_weights[[3]],
    LassoCloseVola           = lasso_weights[[4]],
    LassoClosetoCloseVola    = lasso_weights[[5]],

    XgbCloseVolume           = xgb_weights[[1]],
    XgbCloseEntropy          = xgb_weights[[2]],
    XgbClosetoClosrEntropy   = xgb_weights[[3]],
    XgbCloseVola             = xgb_weights[[4]],
    XgbClosetoCloseVola      = xgb_weights[[5]],

    IntraModelDivergence     = div$IntraModelDivergence %>% sum %>% sqrt,
    LassoErr                 = div$LassoErr %>% sum %>% sqrt,
    XgbErr                   = div$XgbErr %>% sum %>% sqrt
  )
}



apply_weights <- function(data, weights) {


  cntr <- function(v)
    v - min(v)

  (weights[[1]] * cntr(data$NormVolume)) +
  (weights[[2]] * cntr(data$CloseRollingEntropy5)) +
  (weights[[3]] * cntr(data$ClosetoCloseRollingEntropy5)) +
  (weights[[4]] * cntr(data$CloseRollingVola5)) +
  (weights[[5]] * cntr(data$ClosetoCloseRollingVola5))
}




mc_weights_sample <- function(train_data, n_sample)
  1:n_sample %>%
    map(
      ~ get_divergence(
          train_data,
          sample.int(5, replace = T) %>% map_dbl(~ .x / 10),
          sample.int(5, replace = T) %>% map_dbl(~ .x / 10)
        )
    ) %>%
    reduce(rbind)




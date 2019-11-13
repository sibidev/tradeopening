train_xgb <- function(data) {
  recency <- (data$Recency^3) - min(data$Recency^3)
  volume <- (data$NormVolume) - min(data$NormVolume)
  short_entropy <- (data$CloseRollingEntropy5) - min(data$CloseRollingEntropy5)
  short_vola <- (data$CloseRollingVola5) - min(data$CloseRollingVola5)


  w <- recency - short_vola

  xgboost::xgboost(
    data = data %>% select(-IntradayPerf) %>% as.matrix(),
    label = data %>% pull(IntradayPerf),
    missing = NA,
    # weight = w - min(w),
    nrounds = 2^8,
    metrics = "rmse",
    nthread = 12
  )
}

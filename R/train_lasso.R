library(doParallel)
registerDoParallel(12)

train_lasso <- function(data) {
  recency <- (data$Recency^3) - min(data$Recency^3)
  volume <- (data$NormVolume) - min(data$NormVolume)
  short_entropy <- (data$CloseRollingEntropy5) - min(data$CloseRollingEntropy5)
  short_vola <- (data$CloseRollingVola5) - min(data$CloseRollingVola5)
  w <- volume - short_entropy 


  glmnet::cv.glmnet(
    data %>% select(-IntradayPerf) %>% as.matrix(),
    data %>% pull(IntradayPerf) %>% c(),
    # weight = w - min(w),
    nfolds = 2^12,
    parallel = TRUE,
    type.gaussian = "covariance"
  )
}



library(glmnet)
library(xgboost)

importance_xgb <- xgb.importance(model = xgboost_model)
print(importance_xgb)
importance_xgb
xgb.plot.importance(importance_matrix = importance_xgb, top_n=10)


varImp_lasso <- function(object, lambda = NULL, ...) {

  beta <- predict(object, s = lambda, type = "coef")
  if(is.list(beta)) {
    out <- do.call("cbind", lapply(beta, function(x) x[,1]))
    out <- as.data.frame(out)
  } else out <- data.frame(Overall = beta[,1])
  out <- abs(out[rownames(out) != "(Intercept)",,drop = FALSE])
  out
}

importance_lasso<-varImp_lasso(lasso_model, lambda = lasso_model$lambda.min)
importance_lasso




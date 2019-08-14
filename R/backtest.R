get_prediction <- function(test_data, xgboost_model, lasso_model) {
  vals <- test_data %>%
    select(-IntradayPerf) %>%
    as.matrix()

  test_data %>%
    mutate(
      xgb = predict(xgboost_model, vals),
      lasso = glmnet::predict.cv.glmnet(lasso_model, vals, s = "lambda.min"),
      targets = test_data %>% select(IntradayPerf) %>% c()
    )
}

get_backtesting_data <- function(test_data, lasso_model, xgboost_model, eqs)
  test_data %>%
  map(~ get_prediction(
    .x, lasso_model = lasso_model, xgboost_model = xgboost_model
  )) %>%
  map(~ .x %>%
        mutate(PredictedIntradayPerf = (lasso + xgb) / 2)) %>%
  map(~ .x %>%
        mutate(
          Signal = ifelse(
            sign(xgb) == sign(lasso),
            PredictedIntradayPerf,
            0
          )
        )) %>%
  map(~ .x %>%
        mutate(
          Momentum = ifelse(
            sign(xgb) == sign(lasso),
            ((lasso + xgb) - (2 * OpeningPerf)) / 2,
            0
          )
        )) %>%
  map(~ .x %>%
        mutate(
          ConstrainedSignal = ifelse(
            (PredictedIntradayPerf < quantile(OpeningPerf, .2)) ||
            (PredictedIntradayPerf > quantile(OpeningPerf, .8)),
            Momentum,
            0
          )
        )) %>%
  map2(eqs, ~ bind_cols(.x, tail(.y, nrow(.x)))) %>%
  map(~ .x %>% mutate(IntradayPerf = log(Close / Open)))



get_returns <- function(backtesting_data)
  tibble(
    day = 1:nrow(backtesting_data),
    follow_signal_momentum = cumsum(
      sign(backtesting_data$Momentum) * backtesting_data$IntradayPerf
    ),
    follow_signal = cumsum(
      sign(backtesting_data$Signal) * backtesting_data$IntradayPerf
    ),
    buy_and_hold = cumsum(
      TTR::ROC(backtesting_data$Close) %>% coalesce(., 0)
    ),
    constrained = cumsum(
      sign(backtesting_data$ConstrainedSignal) * backtesting_data$IntradayPerf
    ),
    xgb = cumsum(
      sign(backtesting_data$xgb) * backtesting_data$IntradayPerf
    ),
    lasso = cumsum(
      sign(backtesting_data$lasso) * backtesting_data$IntradayPerf
    )
  )

plot_return_densities <- function(returns)
  returns %>%
    reduce(rbind) %>%
    select(-1) %>%
    gather() %>%
    rename(return = value) %>%
    ggplot(aes(return)) +
    facet_wrap(~key) +
    geom_density() +
    theme_bw()


plot_separate_performances <- function(returns, labels)  # takes a while to render
  ggpubr::ggarrange( 
    plotlist =
      returns %>%
      map(~ .x %>%
        ggplot(aes(x = day)) +
        geom_line(aes(y = follow_signal, color = "trade according to signal")) +
        geom_line(aes(y = follow_signal_momentum, color = "trade according to signal + momentum")) +
        geom_line(aes(y = buy_and_hold, color = "buy&hold")) +
        geom_line(aes(y = constrained, color = "constrained signal")) +
        scale_color_manual(values = c(
          "trade according to signal" = "red",
          "trade according to signal + momentum" = "cyan",
          "buy&hold" = "darkblue",
          "constrained signal" = "green"
        )) +
        labs(color = "strategy") +
        theme_bw() +
        ylab("return")),
    labels = labels,
    common.legend = TRUE,
    legend = "bottom",
    hjust = -2
  )

plot_equity <- function(returns) 
  returns %>%
    reduce(rbind) %>%
    group_by(day) %>%
    summarise_all(mean) %>%
    ggplot(aes(x = day)) +
    geom_line(aes(y = follow_signal_momentum), color = "cyan") +
    geom_line(aes(y = follow_signal), color = "red") +
    geom_line(aes(y = constrained), color = "green") +
    geom_line(aes(y = buy_and_hold, color = "darkblue")) +
    ylab("return") +
    theme_bw() +
    theme(legend.position="none")



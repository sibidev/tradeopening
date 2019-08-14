set_meta <- function(path, df) {
  nm <-
    strsplit(path, "/")[[1]] %>%
    tail(1) %>%
    strsplit("[.]") %>%
    unlist()


  add_column(
    Market = nm[2],
    Name = nm[1],
    df
  )
}

get_performance <- function(df) {
  open <- pull(df, Open)
  close <- pull(df, Close)
  volume <- pull(df, Volume)
  volume[volume %in% c(0, NA)] <- 1
  yday_open <- pull(df, Open) %>% lag()
  yday_close <- pull(df, Close) %>% lag()
  two_days_ago_close <- pull(df, Close) %>% lag(2)
  yday_low <- pull(df, Low) %>% lag()
  yday_high <- pull(df, High) %>% lag()

  df %>%
    add_column(
      OpeningPerf = log(open / yday_close),
      IntradayPerf = log(close / open),
      NormLow = log(yday_low / yday_open),
      NormHigh = log(yday_high / yday_open),
      NormVolume = log(volume / sqrt((sum(volume^2)))),
      NormCloseToClose = log(yday_close / two_days_ago_close)
    ) 
}

get_indicators <- function(df) {
  yday_close <- pull(df, Close) %>% lag()
  yday_volume <- pull(df, NormVolume) %>% lag()
  yday_volume[yday_volume %in% c(0, NA)] <- 1

  sma <- function(n)
    TTR::SMA(yday_close, n)

  vwap <- function(n)
    TTR::VWAP(yday_close, yday_volume, n = n)

  emv <- function(n)
    TTR::EMV(df[, c("NormHigh", "NormLow")], yday_volume, n)

  rolling_vola <- function(vec, n)
    vec %>%
      tidyr::replace_na(0) %>%
      zoo::rollapply(
        width = n,
        fill = NA,
        align = "right",
        FUN = sd
      )

  rolling_entropy <- function(vec, n)
    vec %>%
      tidyr::replace_na(0) %>%
      zoo::rollapply(
        width = n,
        fill = NA,
        align = "right",
        FUN = entropy::entropy.ChaoShen
      )


  adx <- function(n)
    TTR::ADX(df[, c("NormHigh", "NormLow", "OpeningPerf")], n)

  money_flow_volume <- function(n)
    TTR::MFI(df[, c("NormHigh", "NormLow", "OpeningPerf")], yday_volume, n)


  df %>%
    add_column(
      ADX5 = adx(5),
      ADX10 = adx(10),
      ADX20 = adx(20),
      ADX40 = adx(40),
      CloseRollingEntropy5 = rolling_entropy(yday_close, 5),
      CloseRollingEntropy10 = rolling_entropy(yday_close, 10),
      CloseRollingEntropy20 = rolling_entropy(yday_close, 20),
      CloseRollingEntropy50 = rolling_entropy(yday_close, 50),
      CloseRollingVola5 = rolling_vola(yday_close, 5),
      CloseRollingVola10 = rolling_vola(yday_close, 10),
      CloseRollingVola20 = rolling_vola(yday_close, 20),
      CloseRollingVola50 = rolling_vola(yday_close, 50),
      ClosetoCloseRollingEntropy5 = rolling_entropy(df$NormCloseToClose, 5),
      ClosetoCloseRollingEntropy10 = rolling_entropy(df$NormCloseToClose, 10),
      ClosetoCloseRollingEntropy20 = rolling_entropy(df$NormCloseToClose, 20),
      ClosetoCloseRollingEntropy50 = rolling_entropy(df$NormCloseToClose, 50),
      ClosetoCloseRollingVola5 = rolling_vola(df$NormCloseToClose, 5),
      ClosetoCloseRollingVola10 = rolling_vola(df$NormCloseToClose, 10),
      ClosetoCloseRollingVola20 = rolling_vola(df$NormCloseToClose, 20),
      ClosetoCloseRollingVola50 = rolling_vola(df$NormCloseToClose, 50),
      EMV5 = emv(5),
      EMV10 = emv(10),
      EMV20 = emv(20),
      MoneyFlow5 = money_flow_volume(5),
      MoneyFlow10 = money_flow_volume(10),
      MoneyFlow20 = money_flow_volume(20),
      MoneyFlow40 = money_flow_volume(40),
      NormSMA5 = log(yday_close / sma(5)),
      NormSMA10 = log(yday_close / sma(10)),
      NormSMA20 = log(yday_close / sma(20)),
      NormSMA50 = log(yday_close / sma(50)),
      NormVWAP5 = log(yday_close / vwap(5)),
      NormVWAP10 = log(yday_close / vwap(10)),
      NormVWAP20 = log(yday_close / vwap(20)),
      PBands5 = TTR::PBands(yday_close, n = 5),
      PBands10 = TTR::PBands(yday_close, n = 10),
      PBands20 = TTR::PBands(yday_close, n = 20),
      Recency = scale(-as.numeric(lubridate::interval(df$Date, lubridate::today()))),
      Three_Seven_d_ratio = TTR::SMA(yday_close, 3) / TTR::SMA(yday_close, 7),
      Seven_Twenty_d_ratio = TTR::SMA(yday_close, 7) / TTR::SMA(yday_close, 20),
      TDI5 = TTR::TDI(yday_close, 5),
      TDI10 = TTR::TDI(yday_close, 10),
      TDI20 = TTR::TDI(yday_close, 20),
      VwapMeanReversion5 = yday_close / vwap(5),
      VwapMeanReversion10 = yday_close / vwap(10),
      VwapMeanReversion20 = yday_close / vwap(20),
      VwapMeanReversion50 = yday_close / vwap(50)
    ) %>%
    na.omit()
}


get_clusters <- function(df)
  df %>%
    select_if(is.numeric) %>%
    mutate(
      Db3Cluster =
        dbscan::hdbscan(select(., -IntradayPerf), minPts = 3)$cluster
    ) %>%
    mutate(
      Km2Cluster =
        kmeans(select(., -IntradayPerf), 2)$cluster
    )

get_training_relevant_features <- function(df)
  df %>%
    select(
      ADX5,
      ADX10,
      ADX20,
      ADX40,
      CloseRollingVola5,
      CloseRollingVola10,
      CloseRollingVola50,
      ClosetoCloseRollingVola5,
      ClosetoCloseRollingVola10,
      ClosetoCloseRollingVola20,
      ClosetoCloseRollingVola50,
      CloseRollingEntropy5,
      CloseRollingEntropy10,
      CloseRollingEntropy20,
      CloseRollingEntropy50,
      ClosetoCloseRollingEntropy5,
      ClosetoCloseRollingEntropy10,
      ClosetoCloseRollingEntropy20,
      ClosetoCloseRollingEntropy50,
      Db3Cluster,
      EMV5,
      EMV10,
      EMV20,
      Km2Cluster,
      MoneyFlow5,
      MoneyFlow10,
      MoneyFlow20,
      MoneyFlow40,
      NormCloseToClose,
      NormHigh,
      NormLow,
      NormSMA5,
      NormSMA10,
      NormSMA20,
      NormVWAP5,
      NormVWAP10,
      NormVWAP20,
      NormVolume,
      OpeningPerf,
      PBands5,
      PBands10,
      PBands20,
      Recency,
      Seven_Twenty_d_ratio,
      TDI5,
      TDI10,
      TDI20,
      Three_Seven_d_ratio,
      VwapMeanReversion5,
      VwapMeanReversion10,
      VwapMeanReversion20,
      VwapMeanReversion50,
      IntradayPerf
    )


prepare_data <- function(df)
  df %>% future_map(
    compose(
      as_tibble,
      scale,
      get_training_relevant_features,
      get_clusters,
      get_indicators,
      get_performance
    )
  )




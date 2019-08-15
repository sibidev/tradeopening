
options(digits=10)

# remove variables from environment
rm(file_list)
rm(full_dataset)
rm(clean_dataset)
rm(equities)
rm(equities_train)
rm(equities_test)

library (MASS)
library(tidyverse)
library(caret)
library(xgboost)
library(lubridate)


# ---------------------- Load data from all Files  --------------------------

# get list of files to be read
file_list <- list.files(path='~//R/GoIT/tradeopening/data')
setwd('~//R/GoIT/tradeopening/data')

#blank dataframe
full_dataset <- data.frame()

# read all files and populate full_dataset
for (i in 1:length(file_list)){
  temp_data <- read.csv(file_list[i], stringsAsFactors = FALSE) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data$Company <- gsub(".DE.csv", "", file_list[i]) #clean the data as needed, in this case I am creating a new column that indicates which file each row of data came from
  full_dataset <- rbind(full_dataset, temp_data) #for each iteration, bind the new data to the building dataset
}

#26 files kept. 4 files removed for small number of observations.

#---------------- Clean null values ----------------------------------

unique(full_dataset$Company)
str(full_dataset)
sum(is.na(full_dataset$Open==TRUE ))
#null value found in Open column

glimpse(full_dataset)

#check for null values in all columns
nrow(full_dataset %>% filter(Open=='null')) # 1054 obs
nrow(full_dataset %>% filter(High =='null'))# 1054 obs
nrow(full_dataset %>% filter(Low  =='null'))# 1054 obs
nrow(full_dataset %>% filter(Close  =='null'))# 1054 obs
nrow(full_dataset %>% filter(Adj.Close  =='null'))# 1054 obs
nrow(full_dataset %>% filter(Volume  =='null'))# 1054 obs
nrow(full_dataset %>% filter(Company  =='null')) #0


#remove observations will null values
clean_dataset <- full_dataset %>% filter(Open!='null')


#check for null values in all columns AFTER CLEAN
nrow(clean_dataset %>% filter(Open=='null')) # 0 obs
nrow(clean_dataset %>% filter(High =='null'))# 0 obs
nrow(clean_dataset %>% filter(Low  =='null'))# 0 obs
nrow(clean_dataset %>% filter(Close  =='null'))# 0 obs
nrow(clean_dataset %>% filter(Adj.Close  =='null'))# 0 obs
nrow(clean_dataset %>% filter(Volume  =='null'))# 0 obs
nrow(clean_dataset %>% filter(Company  =='null')) #0


# Convert numeric columns from Char to Numeric, from Char to Date
# Remove gsub function of ',' removal.
clean_dataset$Date <- as.Date(clean_dataset$Date)
clean_dataset$Open <- as.numeric( clean_dataset$Open  )
clean_dataset$High <- as.numeric( clean_dataset$High  )
clean_dataset$Low <- as.numeric( clean_dataset$Low  )
clean_dataset$Close <- as.numeric( clean_dataset$Close  )
clean_dataset$Volume <- as.numeric( clean_dataset$Volume )
clean_dataset$Adj.Close <- as.numeric( clean_dataset$Adj.Close)



# check datatypes after conversion
str(clean_dataset)

# Create e loop to add the previous_close variables

#add current return and previous day return.



OpeningPerf = log(open / yday_close),
IntradayPerf = log(close / open)


# Add 5 Previous Close, Current Return, Opening Performance, Intraday performance, grouped by Company.



equities <-
  clean_dataset %>%
    group_by(Company) %>%
      mutate( prev_close = lag( x= Close, n = 1L, default = NA, order_by = Date )  )


equities <-
  equities %>%
  group_by(Company) %>%
  mutate( prev_close_2 = lag( x= Close, n = 2L, default = NA, order_by = Date )  )

equities <-
  equities %>%
  group_by(Company) %>%
  mutate( prev_close_3 = lag( x= Close, n = 3L, default = NA, order_by = Date )  )

equities <-
  equities %>%
  group_by(Company) %>%
  mutate( prev_close_4 = lag( x= Close, n = 4L, default = NA, order_by = Date )  )

equities <-
  equities %>%
  group_by(Company) %>%
  mutate( prev_close_5 = lag( x= Close, n = 5L, default = NA, order_by = Date )  )

equities <-
  equities %>%
  group_by(Company) %>%
  mutate( prev_close_6 = lag( x= Close, n = 6L, default = NA, order_by = Date )  )

equities <-
  equities %>%
  group_by( Company ) %>%
  mutate(  curr_return = ((Close/prev_close )-1)*100  )

equities <-
  equities %>%
  group_by( Company ) %>%
  mutate(  prev_return = ((prev_close/prev_close_2 )-1)*100  )

equities <-
  equities %>%
  group_by( Company ) %>%
  mutate(  prev_return_2 = ((prev_close_2/prev_close_3 )-1)*100  )

equities <-
  equities %>%
  group_by( Company ) %>%
  mutate(  prev_return_3 = ((prev_close_3/prev_close_4 )-1)*100  )

equities <-
  equities %>%
  group_by( Company ) %>%
  mutate(  prev_return_4 = ((prev_close_4/prev_close_5 )-1)*100  )

equities <-
  equities %>%
  group_by( Company ) %>%
  mutate(  prev_return_5 = ((prev_close_5/prev_close_6 )-1)*100  )

equities <-
  equities %>%
  group_by( Company ) %>%
  mutate(  opening_perf = ((Open/prev_close )-1)*100  )

equities <-
  equities %>%
  group_by( Company ) %>%
  mutate(  intraday_perf = ((Close/Open )-1)*100  )


equities <-
  equities %>%
  mutate( Direction = ifelse(Close > Open, 1, 0) )

equities <-
  equities %>%
  group_by( Company ) %>%
  mutate(  ma50 = TTR::SMA(prev_close, n = 50)  )

equities <-
  equities %>%
  group_by( Company ) %>%
  mutate(  ma200 = TTR::SMA(prev_close, n = 200)  )

#add volatility
vol_percent = sd(price) / mean(price)

ret <- log(lag(price)) - log(price)
vol <- sd(ret) * sqrt(250) * 100

equities <-
  equities %>%
  group_by( Company ) %>%
  mutate( year = year(Date)  ) %>%
  group_by( year ) %>%
  mutate(mean =  mean(curr_return)) %>%
  mutate(  return_volatility = curr_return - mean(curr_return)   )

CON <- equities %>% filter( Company == '1CON' )%>%
  sd(diff(CON$Close))

?diff

?sd
################################################################

select(equities, -Direction3)

equities <- equities[,-c(14,15)]

equities[,-c(3,4,6,7,8,9)]
equities %>% select(c(Date, Open, Close, Direction))

warnings()
glimpse(equities)

sum(equities$Direction == 1) #66243
sum(equities$Direction == 0) #71770


equities %>% filter(Close > Open)
# remove observations that have null values for the added columns, prev_close, prev_close_1 ...
equities <- equities %>% filter(!is.na(prev_close))%>%
                      filter(!is.na(prev_close_2))%>%
                      filter(!is.na(prev_close_3))%>%
                      filter(!is.na(prev_close_4))%>%
                      filter(!is.na(prev_close_5))%>%
                      filter(!is.na(prev_close_6))%>%
                      filter(!is.na(ma200))


head(equities, n=10)

cor(equities)

str(equities)

str(equities_train)

# create Train & Test datasets
equities_train <- equities %>% filter( Date < as.Date('2016-01-01'))
equities_test <- equities %>% filter(Date >= as.Date('2016-01-01'))


equities$Direction <- as.factor(equities$Direction)
equities_train$Direction <- as.factor(equities_train$Direction)
equities_test$Direction <- as.factor(equities_test$Direction)



# Model creation: LDA
lda_fit=lda(Direction ~ prev_return + prev_return_2 + prev_return_3 + prev_return_4+
              prev_return_5 + opening_perf
            ,data=equities_train)
# add more features, MA, volatility - how much the price flactuated, SD of the time series
# difference of past returns
#work on returns , use returns as predictors, not closing price
#return = prev_close-prev_close_2 in %
#diference between last day close and Open.

# Prediction LDA
lda_pred <- predict (lda_fit , equities_test)


# Test LDA model
table(lda_pred$class, equities_test$Direction)
lda_confmat <- confusionMatrix(lda_pred$class ,as.factor(equities_test$Direction))
lda_confmat$overall





# Model Creation: QDA
qda_fit <- qda(Direction ~ prev_return + prev_return_2 + prev_return_3 + prev_return_4+
                 prev_return_5 + opening_perf
               , data=equities_train   )

# Prediction QDA
qda_pred <- predict (qda_fit , equities_test, type = 'prob')

# Test QDA model
table(qda_pred$class, equities_test$Direction)

qda_confmat <- confusionMatrix(qda_pred$class ,as.factor(equities_test$Direction))

qda_confmat$table
qda_confmat$overall


levels(qda_pred$class)
levels(equities_test$Direction)



##Extreme Gradient Boosting
##tunegrid.xgbDART <- expand.grid(nrounds = 100, max_depth = 10, eta = 0.7,
##                                gamma = 0, subsample = 0.5, colsample_bytree = 0.8,
##                                rate_drop = 0.01, skip_drop = 0.95, min_child_weight = 1)


cv <- trainControl(method = 'cv', number = 10, classProbs = TRUE, allowParallel = TRUE)

str(equities_train)

unique(equities_train$Direction)

levels(equities_train$Direction) <- c('Down', 'Up')
levels(equities_test$Direction) <- c('Down', 'Up')

xgbTree_fit <- caret::train(Direction ~ prev_return + prev_return_2 + prev_return_3 + prev_return_4+
                              prev_return_5 + opening_perf + ma50 + ma200 ,
                            data= equities_train,
                            preProcess = c('center', 'scale'),
                            method = 'xgbTree',
                            trControl = cv##,
                            ##tuneGrid = tunegrid.xgbDART
)



varImp(xgbTree_fit)$importance

#remove non important features
#rerun the models with only the most important features.

xgbTree_pred <- predict(xgbTree_fit, equities_test )
xgbTree_probs <- predict(xgbTree_fit,equities_test,type="prob")
xgbTree_confmat <- confusionMatrix(xgbTree_pred, equities_test$Direction )

xgbTree_confmat$table
xgbTree_confmat$overall

###################################################################

#TODO find if current Open correlated with Prev_close
#Companies has different starting date. Does it affect the model creation?
head(equities)

# Model Creation
# Logistic Regression
# Linear Discriminant Analysis LDA
# Find important variables first then model.
# Quadratic Discriminant Analysis


#add features:
# difference between high and low for each day of the last N days.
# difference between open and close for each aday of the last N days.

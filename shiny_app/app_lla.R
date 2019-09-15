#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(here)
library(tidyverse)
#library(Metrics)


here()
source('src/backtest.R')
returns<-readRDS('data/returns.rds')
test_data<-readRDS('data/test_data.rds')
xgboost_model<-readRDS('models/xgboost_model.rds')
lasso_model<-readRDS('models/lasso_model.rds')
dax_equities<-readRDS('data/dax_equities.rds')
backtesting_data1 <- test_data[1] %>% get_backtesting_data(lasso_model, xgboost_model, dax_equities[1])
backtesting_data2 <- test_data[2] %>% get_backtesting_data(lasso_model, xgboost_model, dax_equities[2])
backtesting_data3 <- test_data[3] %>% get_backtesting_data(lasso_model, xgboost_model, dax_equities[3])

# fit_lasso <- readRDS(here("models", 'lasso.rds'))
# fit_xgb <- readRDS(here("models". 'xgb.rds'))
#...

# Define UI for app
ui <- fluidPage(

  dashboardPage(
    dashboardHeader(title = "Trade the Opening Challenger Models",
                    titleWidth = 450),

    dashboardSidebar(disable = TRUE),

    dashboardBody(
      tags$head(tags$style(HTML('

                              }'))),

      fluidRow(
        column(width = 8,
               box(plotOutput("forecast_plots"),
                   width = NULL),
               box(selectInput("equity", "Choose Equity:",
                               c("1COV" = "1cov"
                                 ,"ADS" = "ads"
                                 ,"ALV" = "alv"
                                 #, ...
                               ))),

               box(plotOutput("diag_plots"),
                   width = NULL)),

        column(width = 4,
               box(selectInput("forecast", "Choose Predictive Model:",
                               c("XGBoost" = "fit_xgb",
                                 "Lasso Regression" = "fit_lasso"

                                 #, ...
                               )),
                   width=NULL),
               box(DT::dataTableOutput("accuracy_table"),
                   width=NULL),
               box(verbatimTextOutput('test_print'),
                   width=NULL))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {



  output$forecast_plots <- renderPlot({
    if (input$forecast == "fit_lasso") {

      returns %>%
        reduce(rbind) %>%
        group_by(day) %>%
        summarise_all(mean) %>%
        ggplot(aes(x = day)) +
        geom_line(aes(y = lasso), color = "cyan") +
        #geom_line(aes(y = follow_signal_momentum), color = "cyan") +
        #geom_line(aes(y = follow_signal), color = "red") +
        #geom_line(aes(y = constrained), color = "green") +
        geom_line(aes(y = buy_and_hold), color = "darkblue") +
        ylab("return") +
        theme_bw() +
        theme(legend.position = "none")

    }
    else if (input$forecast == "fit_xgb") {
      returns %>%
        reduce(rbind) %>%
        group_by(day) %>%
        summarise_all(mean) %>%
        ggplot(aes(x = day)) +
        geom_line(aes(y = xgb), color = "green") +
        #geom_line(aes(y = follow_signal_momentum), color = "cyan") +
        #geom_line(aes(y = follow_signal), color = "red") +
        #geom_line(aes(y = constrained), color = "green") +
        geom_line(aes(y = buy_and_hold), color = "darkblue") +
        ylab("return") +
        theme_bw() +
        theme(legend.position = "none")

    }
    else if (input$forecast == "constrained") {
      returns %>%
        reduce(rbind) %>%
        group_by(day) %>%
        summarise_all(mean) %>%
        ggplot(aes(x = day)) +
        geom_line(aes(y = xgb), color = "green") +
        geom_line(aes(y = lasso), color = "cyan") +
        #geom_line(aes(y = follow_signal), color = "red") +
        geom_line(aes(y = constrained), color = "red") +
        geom_line(aes(y = buy_and_hold), color = "darkblue") +
        ylab("return") +
        theme_bw() +
        theme(legend.position = "none")

    }
  })

  output$diag_plots <- renderPlot({
    if (input$forecast == "fit_lasso") {

      if (input$equity == "1cov") {
        backtesting_data1 %>%
          as.data.frame()%>%
          ggplot(aes(x = Date)) +
          geom_line(aes(y = lasso), color = "cyan") +
          #geom_line(aes(y = xgb), color = "red") +
          geom_line(aes(y = targets), color = "darkblue") +
          ylab("Intraday performance") +
          theme_bw() +
          theme(legend.position = "none")
      }
      else if(input$equity == "ads") {
        backtesting_data2 %>%
          as.data.frame()%>%
          ggplot(aes(x = Date)) +
          geom_line(aes(y = lasso), color = "cyan") +
          #geom_line(aes(y = xgb), color = "red") +
          geom_line(aes(y = targets), color = "darkblue") +
          ylab("Intraday performance") +
          theme_bw() +
          theme(legend.position = "none")
      }
      else if(input$equity == "alv") {
        backtesting_data3 %>%
          as.data.frame()%>%
          ggplot(aes(x = Date)) +
          geom_line(aes(y = lasso), color = "cyan") +
          #geom_line(aes(y = xgb), color = "red") +
          geom_line(aes(y = targets), color = "darkblue") +
          ylab("Intraday performance") +
          theme_bw() +
          theme(legend.position = "none")
      }


    }
    else if (input$forecast == "fit_xgb") {


      if (input$equity == "1cov") {
        backtesting_data1 %>%
          as.data.frame()%>%
          ggplot(aes(x = Date)) +
          #geom_line(aes(y = lasso), color = "cyan") +
          geom_line(aes(y = xgb), color = "green") +
          geom_line(aes(y = targets), color = "darkblue") +
          ylab("Intraday performance") +
          theme_bw() +
          theme(legend.position = "none")
      }
      else if(input$equity == "ads") {
        backtesting_data2 %>%
          as.data.frame()%>%
          ggplot(aes(x = Date)) +
          #geom_line(aes(y = lasso), color = "cyan") +
          geom_line(aes(y = xgb), color = "green") +
          geom_line(aes(y = targets), color = "darkblue") +
          ylab("Intraday performance") +
          theme_bw() +
          theme(legend.position = "none")
      }
      else if(input$equity == "alv") {
        backtesting_data3 %>%
          as.data.frame()%>%
          ggplot(aes(x = Date)) +
          #geom_line(aes(y = lasso), color = "cyan") +
          geom_line(aes(y = xgb), color = "green") +
          geom_line(aes(y = targets), color = "darkblue") +
          ylab("Intraday performance") +
          theme_bw() +
          theme(legend.position = "none")
      }


    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)



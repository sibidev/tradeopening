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

# source(here('src', 'helper_functions.R'))
here()

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
               box(plotOutput("diag_plots"),
                   width = NULL)),

        column(width = 4,
               box(selectInput("forecast", "Choose Predictive Model:",
                               c("Lasso Regression" = "fit_lasso"
                                 #,"XGBoost" = "fit_xgb"
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

#  output$forecast_plots <- renderPlotly({
#        if (input$forecast == "fit_lasso") {

#    }
#   else if (input$forecast == "fit_xgb") {

#    }

#  output$diag_plots <- renderPlot({
#    if (input$forecast == "fit_lasso") {

#    }
#  })
  }

# Run the application
shinyApp(ui = ui, server = server)


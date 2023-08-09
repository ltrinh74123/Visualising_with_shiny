library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)
library(shinythemes)
library(fresh)
library(fontawesome)
file_name = read.csv("data/file_names.csv") %>% unlist %>% as.vector()
time_id = read.csv("data/unique_timeid.csv") %>% unlist %>% as.vector() %>% sort()
# Define UI for application that draws a histogram
js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #E7A03C;
}"'


header <- dashboardHeader(title = "Modelling Volatility")

body = dashboardBody(
  tags$style(js),
  fluidRow(
    box(height = 426, title = "Model Performance Inputs", solidHeader = TRUE, status = "warning",
        selectInput('stock', 'Stock Dataset', choices = file_name, selected = "stock 1"),
        selectInput("timeid", "Time ID", choices = time_id),
        radioButtons('model', 'Type of Model',
                     choices = c( "Linear Regression", "ARMA-GARCH", "HAR-RV") %>% sort(),
                     selected = "ARMA-GARCH"),
        radioButtons("cluster", "Number of clusters", choices = c(1,2,3)), 
        width = 4),
    
  #   box(div(
  #     textOutput("boxText_vol"),
  #     style = paste("font-family: 'Arial', sans-serif;",
  #                   "font-size: 16px;",
  #                   "margin-bottom: 20px;")),
  #     title = "Volatility", plotlyOutput("volatility"), height = 440, width = 8, status = "success", solidHeader = TRUE)
  # ),
  
  box(title = "Volatility inside the Time Buckets", width = 8, solidHeader = T, status = "primary",
      tabBox(
    id = "tabset1", width = 12,
    tabPanel("Within each time ID and stock", "The graph shows the predicted and realised volatility for one time ID in a stock for a chosen model", plotlyOutput("volatility"), 
             tags$style(HTML(".tab-content { height: 300px; overflow-y: hidden; }"))),
    tabPanel("Within each stock", "The graph shows the average predicted and realised volatility in a stock for a chosen model", plotlyOutput("volatility_group"), 
              tags$style(HTML(".tab-content { height: 300px; overflow-y: hidden; }")))
    
  )
  )),
  
  fluidRow(
    box(width = 12,div(
      textOutput("boxText"),
      style = paste("font-family: 'Arial', sans-serif;",
                    "font-size: 14px;",
                    "margin-bottom: 10px;")),
      title = "Metrics based on Stock Clusters", status = "primary", solidHeader = T,
      box(title = "MSE", plotlyOutput("clustered_mse"), width = 4, status = "warning", solidHeader = F),
      box(title = "MAE", plotlyOutput("clustered_mae"), width = 4, status = "warning", solidHeader = F),
      box(title = "QLIKE", plotlyOutput("clustered_qlike"), width = 4, status = "warning", solidHeader = F)
    ))
)


dashboardPage(
  skin = "blue",
  header,
  dashboardSidebar(disable = TRUE),
  body
)

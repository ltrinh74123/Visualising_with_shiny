#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
vol_per_timeid = read.csv("data/models_vol.csv")
all_clusters = read.csv("data/all_clusters.csv")
vol_per_timeid_groups = vol_per_timeid %>% subset(select = -time_id)
grouped_vol = vol_per_timeid_groups %>% group_by(model, stock, time_bucket) %>% 
  reframe(volatility = mean(volatility), pred_volatility = mean(pred_volatility))

#Volatility
# vol_per_timeid$sample = ifelse(vol_per_timeid$time_bucket >= 1 & vol_per_timeid$time_bucket <= 17, "in", "out")
# insample_pred = vol_per_timeid %>% subset(sample == "in")
# vol_per_timeid$sample = ifelse(vol_per_timeid$time_bucket >= 1 & vol_per_timeid$time_bucket <= 16, "in", "out")
# outsample_pred = vol_per_timeid %>% subset(sample == "out")

# Define server logic required to draw a histogram
function(input, output, session) {
  subset_pred = reactive({
    a <- subset(vol_per_timeid, model == input$model & stock == input$stock & time_id == input$timeid)
    return(a)
  })
  
  insample <- reactive({
    ifelse(subset_pred()$time_bucket >= 1 & subset_pred()$time_bucket <= 17, "in", "out")
  })
  
  insample_pred <- reactive({
    subset_pred() %>% subset(insample() == "in")
  })
  
  outsample <- reactive({
    ifelse(subset_pred()$time_bucket >= 1 & subset_pred()$time_bucket <= 16, "in", "out")
  })
  
  outsample_pred <- reactive({
    subset_pred() %>% subset(outsample() == "out")
  })
  
  
  subset_pred_group = reactive({
    a <- subset(grouped_vol, model == input$model & stock == input$stock)
    return(a)
  })
  
  insample_group <- reactive({
    ifelse(subset_pred_group()$time_bucket >= 1 & subset_pred_group()$time_bucket <= 17, "in", "out")
  })
  
  insample_pred_group <- reactive({
    subset_pred_group() %>% subset(insample_group() == "in")
  })
  
  outsample_group <- reactive({
    ifelse(subset_pred_group()$time_bucket >= 1 & subset_pred_group()$time_bucket <= 16, "in", "out")
  })
  
  outsample_pred_group <- reactive({
    subset_pred_group() %>% subset(outsample_group() == "out")
  })
  
  
  
  cluster_df = reactive({
    a <- subset(all_clusters, model == input$model & num_clusters == input$cluster)
    return(a)
  })
  
  
  
  output$volatility <- renderPlotly({
    p <- plot_ly(subset_pred(), y = ~volatility, x = ~time_bucket, 
                 name = 'Realised Volatility\n\n', type = 'scatter', mode = 'lines', 
                 line = list(color = 'black', width = 2), height = 250)
    
    p <- p %>% add_trace( height = 250,data = insample_pred(), y=~pred_volatility, 
                         x = ~time_bucket, name = "In-sample \nPredicted \nVolatility\n\n", 
                         type = 'scatter', mode = 'lines', 
                         line = list(color = 'rgb(205, 12, 24)', 
                                     width = 2)) 
    p <- p %>% add_trace( height = 250, data = outsample_pred(), y=~pred_volatility, 
                         x = ~time_bucket, name = "Out-sample \nPredicted \nVolatility", 
                         line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dash')) 
    p = p %>% layout( xaxis = list(title = "Time Buckets"), 
                      yaxis = list (title = "Volatility")) %>% 
      layout(yaxis = list(range = c(0, max(subset_pred()$volatility,subset_pred()$pred_volatility)+0.0001)))
    p = layout(p,
               yaxis = list(showgrid = FALSE,
                            showline = TRUE,
                            linewidth = 1,
                            linecolor = "black"))
    p = layout(p,
               xaxis = list(range = c(0, 20), tickvals = seq(0, 20, 2), showline = T))
    return (p)
  })
  
  output$volatility_group <- renderPlotly({
    p <- plot_ly(subset_pred_group(), y = ~volatility, x = ~time_bucket, 
                 name = 'Realised Volatility\n\n', type = 'scatter', mode = 'lines', 
                 line = list(color = 'black', width = 2), height = 250)
    
    p <- p %>% add_trace(data = insample_pred_group(), y=~pred_volatility, 
                         x = ~time_bucket, name = "In-sample \nPredicted \nVolatility\n\n", 
                         type = 'scatter', mode = 'lines', 
                         line = list(color = 'rgb(205, 12, 24)', 
                                     width = 2)) 
    p <- p %>% add_trace(data = outsample_pred_group(), y=~pred_volatility, 
                         x = ~time_bucket, name = "Out-sample \nPredicted \nVolatility", 
                         line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dash')) 
    p = p %>% layout( xaxis = list(title = "Time Buckets"), 
                      yaxis = list (title = "Volatility")) %>% 
      layout(yaxis = list(range = c(0, max(subset_pred_group()$volatility,subset_pred_group()$pred_volatility)+0.0001)))
    p = layout(p,
               yaxis = list(showgrid = FALSE,
                            showline = TRUE,
                            linewidth = 1,
                            linecolor = "black"))
    p = layout(p,
               xaxis = list(range = c(0, 20), tickvals = seq(0, 20, 2), showline = T))
    return (p)
  })
  
  output$clustered_mse = renderPlotly({
    p = plot_ly(data = cluster_df(), x = ~cluster, y=~mse, color = ~cluster, type = "box", 
                text = ~stock, colors = "Set1",  boxpoints = "all", jitter = 0.4) %>% 
      layout(yaxis = list(tickformat = ".1e", title = "MSE"), 
             xaxis = list(showline = TRUE, title = "Cluster Types")) %>% 
      layout(yaxis = list(zeroline = FALSE,showline = TRUE )) %>% layout(showlegend = FALSE) 
    return(p)
  })
  
  output$clustered_mae = renderPlotly({
    p = plot_ly(data = cluster_df(), x = ~cluster, y=~mae, color = ~cluster, type = "box", colors = "Set1",text = ~stock,  boxpoints = "all", jitter = 0.4) %>% 
      layout(yaxis = list(tickformat = ".1e", title = "MAE"), 
             xaxis = list(showline = TRUE, title = "Cluster Types")) %>% 
      layout(yaxis = list(zeroline = FALSE, showline = TRUE)) %>% layout(showlegend = FALSE)
    return(p)
  })
  
  output$clustered_qlike = renderPlotly({
    p = plot_ly(data = cluster_df(), x = ~cluster, y=~qlike, color = ~cluster, type = "box", text = ~stock, colors = "Set1",  boxpoints = "all", jitter = 0.4) %>% 
      layout(yaxis = list(title = "QLIKE"), 
             xaxis = list(showline = TRUE, title = "Cluster Types")) %>% 
      layout(yaxis = list(zeroline = FALSE, showline = TRUE)) %>% layout(showlegend = FALSE)
    return(p)
  })
  
  output$boxText <- renderText({
    "The boxplots shows the different metrics based on the model and the number of clustered selected. Hovering over the points will show the corresponding stock"
  })

  
  
}

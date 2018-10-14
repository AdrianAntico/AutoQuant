# Inventory Management and Optimization----------------

project_path <- "C:\\Users\\aantico\\Desktop\\Work\\H20\\Inventory_Optimization"
path <- "C:\\Users\\aantico\\Desktop\\Work\\H20\\Inventory_Optimization\\data"
setwd(path)

# Load functions and libraries
library(plotly)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(data.table)
library(h2o)
library(ggplot2);options(warn = -1)
library(GA)
library(Rcpp)
library(AdrianModelingTools)
source(paste0(project_path,"/Scoring_and_Optimization_Functions.R"))

# Define UI for application----------------

ui <- fluidPage(theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel(title = "Remix Institute: Inventory Optimization"),
  
  # Sidebar with a slider input for number of bins
   sidebarLayout(
     sidebarPanel(actionButton("go", "Optimize", icon(name = "user", lib = "glyphicon")),
       selectInput("sku", "SKU:", SKUS, selected = "85123A", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
       numericInput("weeksOut","Number of FC Weeks:",min = 1,max = 52, value = 52, step = 1),
       numericInput("nSims","Number of Sims:",min = 100000, max = 10000000, value = 500000, step = 100000),
       numericInput("on_hand","Inventory Count:",min = 0, max = 10000, value = 1500, step = 10),
       numericInput("stockOutCost","Cost of Stock-Out:",min = 0, max = 1000000, value = 50, step = 50),
       numericInput("carryCostPerc","Cost of Carry Percent:",min = 0.0, max = 1.0, value = round(0.05/52,5), step = 0.005),
       numericInput("shipping_costS1","Strategy1 Shipping Cost:",min = 0.0, max = 100000, value = 25, step = 10),
       numericInput("shipping_costS2","Strategy2 Shipping Cost:",min = 0.0, max = 100000, value = 25, step = 10)
   , width = 4),
   mainPanel(
     tabsetPanel(
       tabPanel("Optimization Results", DT::dataTableOutput("OptimizationOutput")),
       tabPanel("Cost Plots", 
                plotlyOutput("TCPPlots", height = "400px"),
                plotlyOutput("CCPPlots", height = "400px"),
                plotlyOutput("SOCPlots", height = "400px")),
       tabPanel("Inventory Availability over Time", plotlyOutput("DecrementData", height = "850px"))
     )
   )
  )
)

# Server Side----------------

server <- function(input, output) {

  # Update data for different skus
  simulationData <- eventReactive(input$go,{
    Demand(sku      = input$sku, 
           weeksOut = input$weeksOut, 
           nSims    = input$nSims)
  })
  
  # Compute upper and lower bound for sku quantity
  dataOutput <- eventReactive(input$go,{
    simData <- simulationData()
    
    # Sku Cost
    sku_cost     <- SkuCostData[sku == input$sku, mean(UnitPrice)][[1]]
    
    # Search for range of outcomes
    UpperBound <- QuantityRangeFinder(simData,
                                      input$weeksOut,
                                      input$stockOutCost,
                                      input$carryCostPerc,
                                      binSearchGrowth = 2,
                                      TC_MinWeek = NULL)

    # Search within upper and lower bounds
    StartingQuantity <- MidPointQuantity(UpperBound,
                                         simData,
                                         input$weeksOut,
                                         input$stockOutCost,
                                         input$carryCostPerc)
    
    params <- cbind(SO_COST = input$stockOutCost, 
                    CarryCost = paste0(100*round(input$carryCostPerc,4),"%"))
    quantity <- StartingQuantity[, QWeek := abs(MinWeek - ceiling(input$weeksOut / 2))]
    quantity <- quantity[QWeek == min(QWeek)]
    quantity <- quantity[order(QuantityAvailable)][["QuantityAvailable"]][[1]]
    Q <- length(quantity)
    
    # Run optimization routine
    Parameters <- Optimize(simData,
                           quantity,
                           sku_cost,
                           SO_Cost          = input$stockOutCost, 
                           carryCost        = input$carryCostPerc,
                           MaxOrderWindow   = input$weeksOut,
                           nSims            = input$nSims,
                           sku              = input$sku,
                           on_hand          = input$on_hand,
                           ShippingCostBase = input$shipping_costS1,
                           ShippingCostTest = input$shipping_costS2)
    
    # Remove nonsense data
    FINAL <- Parameters[[1]][ReorderQuantity >= 0][order(-AnnualizedSavingsOverBase)]
    
    # Merge in metadata
    data_collect <- cbind(params, FINAL)
    return(list(data_collect, Parameters[[2]]))
  })
  
  # Charts----------------
  
  # Data that utilizes simulation data for burndown charts
  dataPlotting <- eventReactive(input$go,{
    simData <- simulationData()
    data <- AvailableStocks(simData, 
                           input$on_hand, 
                           input$weeksOut, 
                           c(0.05,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,0.95))
  })
  
  output$DecrementData <- renderPlotly({
    data <- dataPlotting()
    on_hand <- input$on_hand
    
    # Add in the week zero values of starting inventory
    data1 <- data.table(WEEKS_OUT = 0, 
                        Q0.95=on_hand,
                        Q0.9=on_hand,
                        Q0.8=on_hand,
                        Q0.7=on_hand,
                        Q0.6=on_hand,
                        Q0.5=on_hand,
                        Q0.4=on_hand,
                        Q0.3=on_hand,
                        Q0.2=on_hand,
                        Q0.1=on_hand,
                        Q0.05=on_hand)
    data2 <- rbindlist(list(data1,data))
    LineSize = 1.07
    ggplotly(ggplot(data2,aes(x = WEEKS_OUT)) + 
      ChartTheme("lightsteelblue1","navyblue", Size = 15) +
      geom_line(aes(y = data2$Q0.95), color="blue", alpha=0.15, size = LineSize) +
      geom_line(aes(y = data2$Q0.9), color="blue", alpha=0.30, size = LineSize) +
      geom_line(aes(y = data2$Q0.8), color="blue", alpha=0.45, size = LineSize) +
      geom_line(aes(y = data2$Q0.7), color="blue", alpha=0.60, size = LineSize) +
      geom_line(aes(y = data2$Q0.6), color="blue", alpha=0.75, size = LineSize) +
      geom_line(aes(y = data2$Q0.5), color="blue", alpha=0.9, size = (LineSize+0.05)) +
      geom_line(aes(y = data2$Q0.4), color="blue", alpha=0.75, size = LineSize) +
      geom_line(aes(y = data2$Q0.3), color="blue", alpha=0.60, size = LineSize) +
      geom_line(aes(y = data2$Q0.2), color="blue", alpha=0.45, size = LineSize) +
      geom_line(aes(y = data2$Q0.1), color="blue", alpha=0.30, size = LineSize) +
      geom_line(aes(y = data2$Q0.05), color="blue", alpha=0.15, size = LineSize) +
      ylab("Available Quantity") + xlab("Weeks Out") +
      theme(legend.position="none"))
  })
  
  # Data table of optimization results
  output$OptimizationOutput <- DT::renderDataTable({
    data <- dataOutput()[[1]]
    finalOut <- t(data)
    FinalOut <- data.table(Metric = rownames(finalOut), Values = finalOut[,1])
    FinalOut[, Description := "bla bla"]
    
    # Change labels
    set(FinalOut, i = 1, j = 1, value = "Stock-Out Cost")
    set(FinalOut, i = 1, j = 3, value = "This is the cost of a stockout given 
        a stock occurs")
    
    set(FinalOut, i = 2, j = 1, value = "Carrying Cost per Week")
    set(FinalOut, i = 2, j = 3, value = "This is the carrying cost per week, 
        for items such as insurance, taxes, etc.")
    
    set(FinalOut, i = 3, j = 1, value = "SKU Identifier")
    set(FinalOut, i = 3, j = 3, value = "Label for the particular item of 
        interest")
    
    set(FinalOut, i = 4, j = 1, value = "SKU Cost per single unit")
    set(FinalOut, i = 4, j = 3, value = "This is the cost for a single unit 
        of the SKU")
    
    set(FinalOut, i = 5, j = 1, value = "Actual Inventory Available for SKU")
    set(FinalOut, i = 5, j = 3, value = "This is the current inventory level 
        for the particular SKU")
    
    set(FinalOut, i = 7, j = 1, value = "Reorder Amount Now")
    set(FinalOut, i = 7, j = 3, value = "This is the amount you should reorder 
        if you were to order now")
    
    set(FinalOut, i = 8, j = 1, value = "Maximum Inventory Level")
    set(FinalOut, i = 8, j = 3, value = "This is the maximum amount of SKU 
        you should stockpile")
    
    set(FinalOut, i = 9, j = 1, value = "Minimum Inventory Level")
    set(FinalOut, i = 9, j = 3, value = "This is the minimum amount of SKU 
        you should stockpile")
    
    set(FinalOut, i = 11, j = 1, value = "Order Frequency under Best 
        Strategy")
    set(FinalOut, i = 11, j = 3, value = "The order frequency for the best 
        strategy")
    
    set(FinalOut, i = 12, j = 1, value = "Order Frequency under Baseline 
        Strategy")
    set(FinalOut, i = 12, j = 3, value = "The order frequency for the baseline 
        strategy")
    
    set(FinalOut, i = 13, j = 1, value = "Annualized Total Costs")
    set(FinalOut, i = 13, j = 3, value = "This is the total cost for the best 
        strategy annualized")
    
    set(FinalOut, i = 14, j = 1, value = "Savings over baseline strategy")
    set(FinalOut, i = 14, j = 3, value = "This is the annualized savings over 
        the baseline strategy")
    
    # Remove rows
    FinalOut <- FinalOut[Metric != "QuantityAvailableHypothetical" &
                           Metric != "MinCostWeek" & 
                           Metric != "CurrentShippingCosts" &
                           Metric != "BestStrategy" &
                           Metric != "OrderFrequencyBest" &
                           Metric != "Stock-Out Cost" &
                           Metric != "Carrying Cost per Week"]
    
    # Final output
    DT::datatable(FinalOut,
                  options = list(pageLength = 25))
  })
  
  # Cost graphs
  output$TCPPlots <- renderPlotly({
    data <- dataOutput()[[1]]
    data1 <- dataOutput()[[2]]
    x <- plotCharts(data, data1)
    ggplotly(x[[1]])
  })
  output$CCPPlots <- renderPlotly({
    data <- dataOutput()[[1]]
    data1 <- dataOutput()[[2]]
    x <- plotCharts(data, data1)
    p1 <- ggplotly(x[[1]])
    ggplotly(x[[2]])
  })
  output$SOCPlots <- renderPlotly({
    data <- dataOutput()[[1]]
    data1 <- dataOutput()[[2]]
    x <- plotCharts(data, data1)
    ggplotly(x[[3]])
  })
}

# Run the application----------------
shinyApp(ui, server)


# Libraries
library(data.table)
library(ggplot2)
library(lubridate)

##################################
# Functions
##################################

# Generate all plots for target == 1
GeneratePlots <- function(data, target_data, agg_level) {
  
  # Based on the level of specified aggregation, modify source data then aggregate
  if(agg_level == "hour") {
    ConsData <- data[, .(Consumption = mean(Consumption)), by = c(names(data[, c(1:4)]))] 
    targetData <- target_data
  } else if(agg_level == "day") {
    temp <- data[, ConsumptionHour := NULL]
    ConsData <- temp[, .(Consumption = mean(Consumption)), by = c(names(temp[, c(1:3)]))]
    targetData <- target_data[, .(High_Flow_Efficiency = mean(High_Flow_Efficiency),
                                  Low_Flow_Efficiency  = mean(Low_Flow_Efficiency)),
                              by = c(names(target_data[, c(1:2)]))]
  } else if (agg_level == "week") {
    temp1 <- data[, DAY_DATE := floor_date(as.Date(DAY_DATE), unit = "week")]
    ConsData <- temp1[, .(Consumption = mean(Consumption)), by = c(names(temp1[, c(1:3)]))]
    tData1 <- target_data[, INSPDATE := floor_date(as.Date(INSPDATE), unit = "week")]
    targetData <- tData1[, .(High_Flow_Efficiency = mean(High_Flow_Efficiency),
                             Low_Flow_Efficiency  = mean(Low_Flow_Efficiency)),
                         by = c(names(tData1[, c(1,2)]))]
  } else if (agg_level == "month") {
    temp2 <- data[, DAY_DATE := floor_date(as.Date(DAY_DATE), unit = "month")]
    ConsData <- temp2[, .(Consumption = mean(Consumption)), by = c(names(temp2[, c(1:3)]))]
    tData2 <- target_data[, INSPDATE := floor_date(as.Date(INSPDATE), unit = "month")]
    targetData <- tData2[, .(High_Flow_Efficiency = mean(High_Flow_Efficiency),
                             Low_Flow_Efficiency  = mean(Low_Flow_Efficiency)),
                         by = c(names(tData2[, c(1,2)]))]
  }
  
  # Loop through each meter and store plot
  targets <- nrow(targetData)
  plotStore <- list()
  for (i in 1:targets) {
    #i       <- 4 # Max 117
    bad     <- targetData[i, BADGE_NBR][[1]]
    date    <- targetData[i, INSPDATE][[1]]
    effHigh <- targetData[i, High_Flow_Efficiency][[1]]
    effLow  <- targetData[i, Low_Flow_Efficiency][[1]]
    
    # Value of Badge by Date, pre / post inspection
    temp <- ConsData[BADGE_NBR == eval(bad)]
    plotStore[[i]] <- ggplot(temp, aes(x = as.Date(DAY_DATE), y = Consumption)) + geom_line(color = "gray35") + chart_theme + ggtitle(paste0(effHigh, " / ", effLow)) +
      geom_vline(xintercept = date, color = "blue", lwd = 1.25)
  }
  return(list(plotStore, ConsData))
}

# Convert to date function
tempDatesFun <- Vectorize(function(x) {
  strsplit(x, " ")[[1]][1]
})

BackGround <- "lightsteelblue1"
OtherColor <- "navyblue"
Size       <- 15
chart_theme <- theme(plot.background = element_rect(fill = "gray94"),
                     panel.background = element_rect(fill = BackGround, colour = OtherColor, size = 0.25, color = OtherColor),
                     panel.grid.major = element_line(colour = OtherColor, size=0.01, color = "white", linetype = 1),
                     panel.grid.minor = element_line(colour = OtherColor, size=0.01, color= "white", linetype = 1),
                     legend.position = "bottom",
                     legend.title = element_text(color = OtherColor, size=Size, face = "bold"),
                     legend.background = element_rect(fill = "gray95",size = 1, linetype = "solid", color = OtherColor),
                     plot.title=element_text(color = OtherColor, size=Size, face = "bold"),
                     axis.title=element_text(color = OtherColor, size=Size, face = "bold"),
                     axis.text=element_text(colour=OtherColor, face = "bold", angle = 90),
                     axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 20, l = 20)),
                     axis.title.y = element_text(margin = margin(t = 20, r = 20, b = 20, l = 20)),
                     panel.border = element_rect(colour = OtherColor, fill = NA, size = 1.5))

##################################
# R Script
##################################

########################
# Consumption Data Prep
########################

# Load data
Cdata <- fread(file.choose(), header=TRUE)

# Convert datetime to date
Cdata[, DAY_DATE := tempDatesFun(DAY_DATE)]
Cdata[, DAY_DATE := as.Date(DAY_DATE, "%m/%d/%Y")]

# Melt Consumption Data
idCols <- names(Cdata[, c(1:3)])
cols <- 4:ncol(Cdata)
measureCols <- names(Cdata[, ..cols])
data <- melt(data = Cdata, id.vars = idCols, measure.vars = measureCols)

# Change level names
i <- 0
for(lev in levels(data[["variable"]])) {
  i <- i + 1
  if(i == 1) {
    set(data, which(data[["variable"]] == eval(lev)), "variable", "24")
  }
  else {
    set(data, which(data[["variable"]] == eval(lev)), "variable", paste(i-1))
  }
}

# Drop Unused Levels
data[, variable := droplevels(variable)]

# Change Names of Consumption Data
setnames(data, c("ENTITY_KEY1", "variable", "value"), c("BADGE_NBR", "ConsumptionHour", "Consumption"))

# Convert ConsumptionHour to Numeric
data[, ConsumptionHour := as.numeric(as.character(ConsumptionHour))]

# rows: 9,043,536
# nrow(data)  

#######################
# Inspection Data Prep
#######################

# Load Data
INSP <- fread(file.choose(), header = TRUE)

# Convert to date
INSP[, INSPDATE := tempDatesFun(INSPDATE)]
INSP[, INSPDATE := as.Date(INSPDATE, "%m/%d/%Y")]

# Add possible target variable
INSP[, Target := ifelse((High_Flow_Efficiency + High_Flow_Efficiency)/2 < 0.97 | (High_Flow_Efficiency + High_Flow_Efficiency)/2 > 1.03, 1, 0)]

# Create data for plots
target_data <- INSP[Target == 1, c("BADGE_NBR", "INSPDATE", "High_Flow_Efficiency", "Low_Flow_Efficiency")]

# High flow + low flow / 2 iff +/- 1.03 0.97
# Hourly / daily / weekly / monthly
#  - 

#####################################################
# Quick Plots on Consumption Data for a Single Meter
#####################################################

# Hourly plots and data
Hourly <- GeneratePlots(data, target_data, agg_level = "hour")
HourlyPlots <- Hourly[[1]]
HourlyData <- Hourly[[2]]

# Daily plots and data
Daily <- GeneratePlots(data, target_data, agg_level = "day")
DailyPlots <- Daily[[1]]
DailyData <- Daily[[2]]

# Weekly plots and data
Weekly <- GeneratePlots(data, target_data, agg_level = "week")
WeeklyPlots <- Weekly[[1]]
WeeklyData <- Weekly[[2]]

# Month plots and data
Monthly <- GeneratePlots(data, target_data, agg_level = "month")
MonthlyPlots <- Monthly[[1]]
MonthlyData <- Monthly[[2]]

# Value of Badge by Date, pre inspection
# temp <- data[BADGE_NBR == eval(bad) & DAY_DATE <= eval(date)][order(DAY_DATE, ConsumptionHour)]
# ggplot(temp, aes(x = DAY_DATE, y = Consumption)) + geom_line() + chart_theme + ggtitle(paste0(effHigh, " / ", effLow))
# 
# # Value of Badge by date for specific hour
# temp <- data[BADGE_NBR == eval(bad) & DAY_DATE <= eval(date) & ConsumptionHour == 11]
# ggplot(temp, aes(x = DAY_DATE, y = Consumption)) + geom_line() + chart_theme + ggtitle(paste0("High: ",effHigh, " / ","Low: ", effLow))
# 
# # Value of Badge by time of day
# temp <- data[BADGE_NBR == eval(bad) & DAY_DATE <= eval(date)]
# ggplot(temp, aes(x = ConsumptionHour, y = Consumption)) + geom_point() + chart_theme + ggtitle(paste0(effHigh, " / ", effLow))

##################################
# Incorporate Other Data
##################################

# Load Other Data Sets
CMP <- fread(file.choose(), header = TRUE)

# Metadata
names(CMP)   # BADGE_NBR
names(INSP)  # BADGE_NBR

# Merge data sets 1
data <- merge(Cdata, CMP, by = "BADGE_NBR", all.x = TRUE)

data[, DAY_DATE := as.Date(DAY_DATE, "%m/%d/%Y")]

# Merge data sets 2
data <- merge(data, INSP, by.x = c("BADGE_NBR","DAY_DATE"), by.y = c("BADGE_NBR","INSPDATE"), all.x = TRUE)
nrow(data)

##################################################

# Check for meters not inspected
# For duplicated inspected meters:
#   1. All zeros - remove?
#   2. single positive, all others zero: remove zeros?
#   3. Multiple positive, multiple zeros: average out positives & remove zeros?
#   4. All positive: average out positives?

# Metrics
# HFE = Registered Flow / Actual Flow

# Anomaly detection
#   1. Hourly
#   2. Aggregate

# Prediction possibilities:
#   1. Binary classification: either fails
#       +/- 3% for both
#   2. Multi-label: predict both, poss outcomes: (0,0), (1,0), (0,1), (1,1)
#   3. Predict actual numeric values for both

# Features:

# CMP Data
#   1. Age of meter
#   2. Meter Type
#   3. Manufactuer
#   4. Model
#   5. Size

# Consumption Data
#   1. Hourly
#   2. Aggregate
#     Time series features: lags, rolling stats, consumption data. Derived metric: avg daily consumption
#   Factors

##################################################

# Perfect
flow <- data[!is.na(High_Flow_Efficiency), c("High_Flow_Efficiency", "Low_Flow_Efficiency")]
flow[, rowNum := 1:.N]
flow[, Diff := High_Flow_Efficiency - Low_Flow_Efficiency]
# 412 records of inspections

ggplot(flow, aes(x = rowNum)) +
  geom_line(y = flow[["High_Flow_Efficiency"]], color = "red") + 
  geom_line(y = flow[["Low_Flow_Efficiency"]], color = "blue")

ggplot(flow, aes(x = rowNum)) +
  geom_line(y = flow[["Diff"]], color = "blue")
  
str(data)

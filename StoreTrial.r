## Load required libraries and datasets
library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)
library(zoo)
# Read in data
filePath<-("D:/DataProject/venv/Quantum/import/")
data <- fread(paste0(filePath,"QVI_data.csv"))

## Select control stores

#### Calculate measures over time for each store
# Add a new month ID column in the data with the format yyyymm.
data <- transform(data, YEARMONTH = format(DATE, "%Y%m"))
# inspecting the contents of colomn YEARMONTH
head(data[, c("DATE", "YEARMONTH")][1:10, ])

# calculates various metrics over time for each store and month, including total sales, number of customers, transactions per customer, chips per customer, and average price per unit. 
measureOverTime <- data[, .(totSales =sum(TOT_SALES) ,
    nCustomers = n_distinct(LYLTY_CARD_NBR),
    nTxnPerCust =n_distinct(TXN_ID) / n_distinct(LYLTY_CARD_NBR),
    nChipsPerTxn =sum(PROD_QTY) / n_distinct(LYLTY_CARD_NBR),
    avgPricePerUnit =mean(TOT_SALES / PROD_QTY) )
    , by =.(STORE_NBR, YEARMONTH) ][order(desc(YEARMONTH))]   
# inspecting the contents of measureOverTime data
str(measureOverTime)

#### Filter to the pre-trial period and stores with full observation periods

storesWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in%
storesWithFullObs, ]


# Function to calculate the correlation between a control store and a trial store based on a specific measure

calculateCorrelation <- function(inputTable, metricCol, storeComparison) {
  calcCorrTable <- data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
  storeNumbers <- unique(inputTable$STORE_NBR)
  
  for (i in storeNumbers) {
    metric1 <- inputTable[inputTable$STORE_NBR == storeComparison, eval(metricCol)]
    metric2 <- inputTable[inputTable$STORE_NBR == i, eval(metricCol)]
    corr_measure <- cor(metric1, metric2)
    calculatedMeasure <- data.table("Store1" = storeComparison,
                                    "Store2" = i,
                                    "corr_measure" = corr_measure)
    calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
  }
  
  return(calcCorrTable)
}
# Function to calculate the magnitude-based distance between a control store and other stores based on a specified metric.
calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
  calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(), measure = numeric())
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison,
                                  "Store2" = i,
                                  "YEARMONTH" = inputTable[STORE_NBR == storeComparison, YEARMONTH],
                                  "measure" = abs(inputTable[STORE_NBR == storeComparison, eval(metricCol)] - inputTable[STORE_NBR == i, eval(metricCol)])
    )
    calcDistTable <- rbind(calcDistTable, calculatedMeasure)
  }

  #### Standardise the magnitude distance so that the measure ranges from 0 to 1
  minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)),
                              by = c("Store1", "YEARMONTH")]
  distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (measure - minDist) / (maxDist - minDist)]
  
  finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
  return(finalDistTable)
}
# Function to calculate the control store's score and define the control store based on the control store's score.

identifyControlStore <- function(trial_store, preTrialMeasures) {
  # Calculate correlations and magnitudes for trial_store
  corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
  corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)
  distances_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
  distances_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)
  
  # Calculate combined scores 
  corr_weight <- 0.5
  score_nSales <- merge(corr_nSales, distances_nSales, by = c("Store1", "Store2"))[, scoreNSales := 0.5 * corr_measure + 0.5 * mag_measure]
  score_nCustomers <- merge(corr_nCustomers, distances_nCustomers, by = c("Store1", "Store2"))[, scoreNCust := 0.5 * corr_measure + 0.5 * mag_measure]
  
  # Combine scores and calculate final control score
  score_Control <- merge(score_nSales, score_nCustomers, by = c("Store1", "Store2"))
  score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]
  
  # Sort score_Control in descending order by finalControlScore
  sorted_score_Control <- score_Control[order(-finalControlScore)]
  
  # Find the second-highest finalControlScore
  second_highest_score <- sorted_score_Control$finalControlScore[2]
  
# Find the store number(s) with the second-highest finalControlScore
  stores_with_second_highest_score <- sorted_score_Control$Store2[sorted_score_Control$finalControlScore == second_highest_score]
  
  return(stores_with_second_highest_score)
}

controlStore <- identifyControlStore(77, preTrialMeasures)
print(controlStore)
trial_store <- 77
control_store <- 233

measureOverTimeSales <- measureOverTime
# Convert YEARMONTH to numeric for calculations
measureOverTimeSales$YEARMONTH <- as.numeric(as.character(measureOverTimeSales$YEARMONTH))

pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,
                                                        "Trial",
                                                        ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
]

# Create the plot using ggplot2
p <-ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
      geom_line() +
      labs(x = "Month of operation", y = "Total sales", title = "Total sales by month") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
ggsave("salesTvsC.png", plot = p, width = 12, height = 5, units = "in")
# number of customers over time in trial, control, and other stores
measureOverTimeCusts <- measureOverTime

# Convert YEARMONTH to numeric for calculations
measureOverTimeCusts$YEARMONTH <- as.numeric(as.character(measureOverTimeCusts$YEARMONTH))

pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store,
                                                        "Trial",
                                                        ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, avgCustom := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]


# Create the plot using ggplot2
ggplot(pastCustomers, aes(TransactionMonth, avgCustom, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "customer count", title = "customer count by month")

#### Scale pre-trial control sales to match pre-trial trial store sales

scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store &
YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store &
YEARMONTH < 201902, sum(totSales)]
#### Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][
  ,
  controlSales := ifelse(YEARMONTH < 201902, totSales * scalingFactorForControlSales, totSales)
]

head(scaledControlSales)
# Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(
  measureOverTime[STORE_NBR == trial_store , ],
  scaledControlSales,
  by = "YEARMONTH"
)[, percentageDiff :=((totSales.x - controlSales) / totSales.x) * 100]
percentageDiff$TransactionMonth <- as.Date(paste(substr(percentageDiff$YEARMONTH, 1, 4), substr(percentageDiff$YEARMONTH, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")
# Create the plot using ggplot2
p <-ggplot(percentageDiff, aes(x = TransactionMonth, color = Store_type)) +
  geom_line(aes(y = totSales.x, color = "Trial")) +
  geom_line(aes(y = controlSales, color = "Control")) +
  labs(x = "Year and Month", y = "Total Sales", title = "Trial vs Control Sales Over Time") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_manual(values = c("Trial" = "blue", "Control" = "red")) +
  theme_minimal()
ggsave("salesTvsC.png", plot = p, width = 12, height = 5, units = "in")
print(percentageDiff[1:12, c("YEARMONTH", "STORE_NBR.x", "STORE_NBR.y", "percentageDiff")])

# Checking if there is a significant difference between the trial store and control store based on total sales.
mean_trial <- mean(percentageDiff[STORE_NBR.x == trial_store & YEARMONTH > 201902]$totSales.x)
mean_control <- mean(percentageDiff[YEARMONTH > 201902]$controlSales)
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
df <- 7
t_values <- (mean_trial - mean_control) / (stdDev / sqrt(8))
critical_t <- qt(0.95, df)
if (abs(t_values) > critical_t) {
cat("The difference between the mean trial and control is statistically significant.\n")
} else {
cat("The difference between the mean trial and control is not statistically significant.\n")
}
str(percentageDiff)


#### Trial and control store total sales
filtered_data <- percentageDiff[YEARMONTH < 201905 & YEARMONTH > 201901, ]
# Convert YEARMONTH to numeric
filtered_data$YEARMONTH <- as.Date(as.yearmon(filtered_data$YEARMONTH, "%Y%m"))

p <-ggplot(data = filtered_data, aes(x = YEARMONTH, y = totSales.x)) +
  geom_line(aes(color = "Trial Store")) +
  geom_line(aes(y = controlSales, color = "Control Store")) +
  labs(title = "Total Sales by Month",
       x = "Month",
       y = "Total Sales") +
  scale_color_manual(name = 'Store Type',  
                     values = c("Trial Store" = "#3F69AE", "Control Store" = "#F2B418")) + 
  theme_minimal() +
  theme(legend.position = "right")
ggsave("salesTvsC1.png", plot = p, width = 8, height = 5, units = "in")


#### Scale pre-trial control nCustomers to match pre-trial trial store nCustomers 
scalingFactorForControlnCustomers <- preTrialMeasures[STORE_NBR == trial_store &
YEARMONTH < 201902, sum(nCustomers)]/preTrialMeasures[STORE_NBR == control_store &
YEARMONTH < 201902, sum(nCustomers)]
#### Apply the scaling factor
measureOverTimeCusts <- measureOverTime
scaledControlnCustomers <- measureOverTimeCusts[STORE_NBR == control_store, ][ ,
controlnCustomers := nCustomers * scalingFactorForControlnCustomers ]
# Calculate the percentage difference between scaled control nCustomers and trial nCustomers
percentageDiff <- merge(
  measureOverTimeCusts[STORE_NBR == trial_store , ],
  scaledControlnCustomers,
  by = "YEARMONTH"
)[, percentageDiff :=((nCustomers.x - controlnCustomers) / nCustomers.x) * 100]
print(percentageDiff[1:12, c("YEARMONTH", "STORE_NBR.x", "STORE_NBR.y", "percentageDiff")])

# Checking if there is a significant difference between the trial store and control store based on total sales.
mean_trial <- mean(percentageDiff[STORE_NBR.x == trial_store & YEARMONTH > 201902]$nCustomers.x)
mean_control <- mean(percentageDiff[YEARMONTH > 201902]$controlnCustomers)
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
df <- 7
t_values <- (mean_trial - mean_control) / (stdDev / sqrt(8))
critical_t <- qt(0.95, df)
if (abs(t_values) > critical_t) {
cat("The difference between the mean trial and control is statistically significant.\n")
} else {
cat("The difference between the mean trial and control is not statistically significant.\n")
}

#### Trial and control store total sales
filtered_data <- percentageDiff[YEARMONTH < 201905 & YEARMONTH > 201901, ]
# Convert YEARMONTH to numeric
filtered_data$YEARMONTH <- as.Date(as.yearmon(filtered_data$YEARMONTH, "%Y%m"))
# Create the line chart 
p <-ggplot(data = filtered_data, aes(x = YEARMONTH, y = nCustomers.x)) +
  geom_line(aes(color = "Trial Store")) +
  geom_line(aes(y = controlnCustomers, color = "Control Store")) +
  labs(title = "Customer count by Month",
       x = "Month",
       y = "Customer count") +
  scale_color_manual(name = 'Store Type',  
                     values = c("Trial Store" = "#3F69AE", "Control Store" = "#F2B418")) + 
  theme_minimal() +
  theme(legend.position = "right")
ggsave("customerTvsC1.png", plot = p, width = 8, height = 5, units = "in")

## Trial store 86
controlStore <- identifyControlStore(86, preTrialMeasures)
print(controlStore)
trial_store <- 86
control_store <- 155
#### Visual checks on trends based on the drivers
measureOverTimeSales <- measureOverTime
# Convert YEARMONTH to numeric for calculations
measureOverTimeSales$YEARMONTH <- as.numeric(as.character(measureOverTimeSales$YEARMONTH))
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,
                                                        "Trial",
                                                        ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]

# Create the plot using ggplot2
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

# number of customers over time in trial, control, and other stores
measureOverTimeCusts <- measureOverTime

# Convert YEARMONTH to numeric for calculations
measureOverTimeCusts$YEARMONTH <- as.numeric(as.character(measureOverTimeCusts$YEARMONTH))

pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store,
                                                        "Trial",
                                                        ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, avgCustom := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]


# Create the plot using ggplot2
ggplot(pastCustomers, aes(TransactionMonth, avgCustom, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "customer count", title = "customer count by month")

#### Scale pre-trial control sales to match pre-trial trial store sales

scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store &
YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store &
YEARMONTH < 201902, sum(totSales)]
#### Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ ,
controlSales := totSales * scalingFactorForControlSales]

# Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(
  measureOverTime[STORE_NBR == trial_store , ],
  scaledControlSales,
  by = "YEARMONTH"
)[, percentageDiff :=((totSales.x - controlSales) / totSales.x) * 100]
print(percentageDiff[1:12, c("YEARMONTH", "STORE_NBR.x", "STORE_NBR.y", "percentageDiff")])

# Checking if there is a significant difference between the trial store and control store based on total sales.
mean_trial <- mean(percentageDiff[STORE_NBR.x == trial_store & YEARMONTH > 201902]$totSales.x)
mean_control <- mean(percentageDiff[YEARMONTH > 201902]$controlSales)
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
df <- 7
t_values <- (mean_trial - mean_control) / (stdDev / sqrt(8))
critical_t <- qt(0.95, df)
if (abs(t_values) > critical_t) {
cat("The difference between the mean trial and control is statistically significant.\n")
} else {
cat("The difference between the mean trial and control is not statistically significant.\n")
}

#### Trial and control store total sales
filtered_data <- percentageDiff[YEARMONTH < 201905 & YEARMONTH > 201901, ]
# Convert YEARMONTH to numeric
filtered_data$YEARMONTH <- as.Date(as.yearmon(filtered_data$YEARMONTH, "%Y%m"))
# Create the line chart 
p <-ggplot(data = filtered_data, aes(x = YEARMONTH, y = totSales.x)) +
  geom_line(aes(color = "Trial Store")) +
  geom_line(aes(y = controlSales, color = "Control Store")) +
  labs(title = "Total Sales by Month",
       x = "Month",
       y = "Total Sales") +
  scale_color_manual(name = 'Store Type', 
                     values = c("Trial Store" = "#3F69AE", "Control Store" = "#F2B418")) +  
  theme_minimal() +
  theme(legend.position = "right")
ggsave("salesTvsC2.png", plot = p, width = 8, height = 5, units = "in")

#### Scale pre-trial control nCustomers to match pre-trial trial store nCustomers 
scalingFactorForControlnCustomers <- preTrialMeasures[STORE_NBR == trial_store &
YEARMONTH < 201902, sum(nCustomers)]/preTrialMeasures[STORE_NBR == control_store &
YEARMONTH < 201902, sum(nCustomers)]
#### Apply the scaling factor
measureOverTimeCusts <- measureOverTime
scaledControlnCustomers <- measureOverTimeCusts[STORE_NBR == control_store, ][ ,
controlnCustomers := nCustomers * scalingFactorForControlnCustomers ]
# Calculate the percentage difference between scaled control nCustomers and trial nCustomers
percentageDiff <- merge(
  measureOverTimeCusts[STORE_NBR == trial_store , ],
  scaledControlnCustomers,
  by = "YEARMONTH"
)[, percentageDiff :=((nCustomers.x - controlnCustomers) / nCustomers.x) * 100]
print(percentageDiff[1:12, c("YEARMONTH", "STORE_NBR.x", "STORE_NBR.y", "percentageDiff")])

# Checking if there is a significant difference between the trial store and control store based on total sales.
mean_trial <- mean(percentageDiff[STORE_NBR.x == trial_store & YEARMONTH > 201902]$nCustomers.x)
mean_control <- mean(percentageDiff[YEARMONTH > 201902]$controlnCustomers)
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
df <- 7
t_values <- (mean_trial - mean_control) / (stdDev / sqrt(8))
critical_t <- qt(0.95, df)
if (abs(t_values) > critical_t) {
cat("The difference between the mean trial and control is statistically significant.\n")
} else {
cat("The difference between the mean trial and control is not statistically significant.\n")
}
#### Trial and control store number of cutomer in trial period
filtered_data <- percentageDiff[YEARMONTH < 201905 & YEARMONTH > 201901, ]
# Convert YEARMONTH to numeric
filtered_data$YEARMONTH <- as.Date(as.yearmon(filtered_data$YEARMONTH, "%Y%m"))
# Create the line chart 
p <-ggplot(data = filtered_data, aes(x = YEARMONTH, y = nCustomers.x)) +
  geom_line(aes(color = "Trial Store")) +
  geom_line(aes(y = controlnCustomers, color = "Control Store")) +
  labs(title = "Customer count by Month",
       x = "Month",
       y = "Customer count") +
  scale_color_manual(name = 'Store Type',  
                     values = c("Trial Store" = "#3F69AE", "Control Store" = "#F2B418")) +  
  theme_minimal() +
  theme(legend.position = "right")
ggsave("customerTvsC2.png", plot = p, width = 8, height = 5, units = "in")

## Trial store 88
controlStore <- identifyControlStore(88, preTrialMeasures)
print(controlStore)
trial_store <- 88
control_store <- 237
print("88")
#### Visual checks on trends based on the drivers

measureOverTimeSales <- measureOverTime
# Convert YEARMONTH to numeric for calculations
measureOverTimeSales$YEARMONTH <- as.numeric(as.character(measureOverTimeSales$YEARMONTH))
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,
                                                        "Trial",
                                                        ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]

# Create the plot using ggplot2
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

# number of customers over time in trial, control, and other stores
measureOverTimeCusts <- measureOverTime

# Convert YEARMONTH to numeric for calculations
measureOverTimeCusts$YEARMONTH <- as.numeric(as.character(measureOverTimeCusts$YEARMONTH))

pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store,
                                                        "Trial",
                                                        ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, avgCustom := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]


# Create the plot using ggplot2
ggplot(pastCustomers, aes(TransactionMonth, avgCustom, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "customer count", title = "customer count by month")

#### Scale pre-trial control sales to match pre-trial trial store sales

scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store &
YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store &
YEARMONTH < 201902, sum(totSales)]
#### Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ ,
controlSales := totSales * scalingFactorForControlSales]

# Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(
  measureOverTime[STORE_NBR == trial_store , ],
  scaledControlSales,
  by = "YEARMONTH"
)[, percentageDiff :=((totSales.x - controlSales) / totSales.x) * 100]
print(percentageDiff[1:12, c("YEARMONTH", "STORE_NBR.x", "STORE_NBR.y", "percentageDiff")])

# Checking if there is a significant difference between the trial store and control store based on total sales.
mean_trial <- mean(percentageDiff[STORE_NBR.x == trial_store & YEARMONTH > 201902]$totSales.x)
mean_control <- mean(percentageDiff[YEARMONTH > 201902]$controlSales)
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
df <- 7
t_values <- (mean_trial - mean_control) / (stdDev / sqrt(8))
critical_t <- qt(0.95, df)
if (abs(t_values) > critical_t) {
cat("The difference between the mean trial and control is statistically significant.\n")
} else {
cat("The difference between the mean trial and control is not statistically significant.\n")
}
#### Trial and control store total sales
filtered_data <- percentageDiff[YEARMONTH < 201905 & YEARMONTH > 201901, ]
# Convert YEARMONTH to numeric
filtered_data$YEARMONTH <- as.Date(as.yearmon(filtered_data$YEARMONTH, "%Y%m"))
# Create the line chart 
p <-ggplot(data = filtered_data, aes(x = YEARMONTH, y = totSales.x)) +
  geom_line(aes(color = "Trial Store")) +
  geom_line(aes(y = controlSales, color = "Control Store")) +
  labs(title = "Total Sales by Month",
       x = "Month",
       y = "Total Sales") +
  scale_color_manual(name = 'Store Type',  
                     values = c("Trial Store" = "#3F69AE", "Control Store" = "#F2B418")) +  
  theme_minimal() +
  theme(legend.position = "right")
ggsave("salesTvsC3.png", plot = p, width = 8, height = 5, units = "in")
#### Scale pre-trial control nCustomers to match pre-trial trial store nCustomers 
scalingFactorForControlnCustomers <- preTrialMeasures[STORE_NBR == trial_store &
YEARMONTH < 201902, sum(nCustomers)]/preTrialMeasures[STORE_NBR == control_store &
YEARMONTH < 201902, sum(nCustomers)]
#### Apply the scaling factor
measureOverTimeCusts <- measureOverTime
scaledControlnCustomers <- measureOverTimeCusts[STORE_NBR == control_store, ][ ,
controlnCustomers := nCustomers * scalingFactorForControlnCustomers ]
# Calculate the percentage difference between scaled control nCustomers and trial nCustomers
percentageDiff <- merge(
  measureOverTimeCusts[STORE_NBR == trial_store , ],
  scaledControlnCustomers,
  by = "YEARMONTH"
)[, percentageDiff :=((nCustomers.x - controlnCustomers) / nCustomers.x) * 100]
print(percentageDiff[1:12, c("YEARMONTH", "STORE_NBR.x", "STORE_NBR.y", "percentageDiff")])

# Checking if there is a significant difference between the trial store and control store based on total sales.
mean_trial <- mean(percentageDiff[STORE_NBR.x == trial_store & YEARMONTH > 201902]$nCustomers.x)
mean_control <- mean(percentageDiff[YEARMONTH > 201902]$controlnCustomers)
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
df <- 7
t_values <- (mean_trial - mean_control) / (stdDev / sqrt(8))
critical_t <- qt(0.95, df)
if (abs(t_values) > critical_t) {
cat("The difference between the mean trial and control is statistically significant.\n")
} else {
cat("The difference between the mean trial and control is not statistically significant.\n")
}
#### Trial and control store number of cutomer in trial period
filtered_data <- percentageDiff[YEARMONTH < 201905 & YEARMONTH > 201901, ]
# Convert YEARMONTH to numeric
filtered_data$YEARMONTH <- as.Date(as.yearmon(filtered_data$YEARMONTH, "%Y%m"))
# Create the line chart 
p <-ggplot(data = filtered_data, aes(x = YEARMONTH, y = nCustomers.x)) +
  geom_line(aes(color = "Trial Store")) +
  geom_line(aes(y = controlnCustomers, color = "Control Store")) +
  labs(title = "Customer count by Month",
       x = "Month",
       y = "Customer count") +
  scale_color_manual(name = 'Store Type',  
                     values = c("Trial Store" = "#3F69AE", "Control Store" = "#F2B418")) +  
  theme_minimal() +
  theme(legend.position = "right")
ggsave("customerTvsC3.png", plot = p, width = 8, height = 5, units = "in")

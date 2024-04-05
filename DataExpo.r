library(knitr)
library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)
library(splitstackshape)
library(tm)
library(tidyr)
library(readr)

# Read the data from the CSV file
filePath<-("D:/DataProject/venv/Quantum/import/")
transactionData <- fread(paste0(filePath,"QVI_transaction_data.csv")) 
customerData <- fread(paste0(filePath,"QVI_purchase_behaviour.csv"))

# Print the structure of the 'transactionData' data table.
str(transactionData)
# Display the first few rows of the 'transactionData' data table.
head(transactionData)

# Convert the 'DATE' column in the 'transactionData' data table from numeric to Date format.
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

#### Examine PROD_NAME
head(transactionData$PROD_NAME, n = 10)
# Print the number of unique values
print(length(unique(transactionData$PROD_NAME)))

# Create a data table 'productWords' containing unique words extracted from the 'PROD_NAME' column in 'transactionData'.
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), " ")))

# Rename the column of 'productWords' to 'words'.
setnames(productWords, 'words')

# Remove words with digits from the 'productWords' data table.
productWords <- productWords[!grepl("[0-9]", words)]
# Remove special characters from the 'words' column in the 'productWords' data table.
productWords <- productWords[!grepl("[^A-Za-z]", words), ]

# Text preprocessing: Convert to lowercase
productWords$words <- tolower(productWords$words)
# Remove leading and trailing whitespace from the 'words' column
productWords$words <- trimws(productWords$words)
# Filter out rows with non-empty cells (words with more than 0 characters)
productWords <- productWords[nchar(productWords$words) > 0, ]
#### sorting them by this frequency in order of highest to lowest frequency
productWords[, .N, words][order(N, decreasing = TRUE)]


#### Remove salsa products
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]


#### Summarise the data to check for nulls and possible outliers 
summary(transactionData)


# Print the rows where 'PROD_QTY' is equal to 200
prodQtyOutlier <-transactionData[transactionData$PROD_QTY == 200, ]
print(prodQtyOutlier)

# Filter the dataset to find the transaction where 'LYLTY_CARD_NBR' is equal to 226000
customerOutlier <-transactionData[transactionData$LYLTY_CARD_NBR == 226000, ]
print(customerOutlier)
#### Filter out the customer based on the loyalty card number 
transactionData <- transactionData[transactionData$LYLTY_CARD_NB != 226000, ]
summary(transactionData)

#### Count the number of transactions by date
transactionData[, .N, by = DATE]

# Create a sequence of dates from July 1, 2018, to June 30, 2019
date_sequence <- seq(from = as.Date("2018-07-01"), to = as.Date("2019-06-30"), by = "day")

# Merge the sequence of dates with the transactionData to fill in missing dates
transactions_by_date <- data.frame(DATE = date_sequence) %>%
  left_join(transactionData %>%
              group_by(DATE) %>%
              summarise(Num_Transactions = n()), by = "DATE")


#### Plot transactions over time 

ggplot(transactions_by_date, aes(x = DATE, y = Num_Transactions)) +
  geom_line() +
  labs(x = "Date", y = "Number of Transactions", title = "Number of Transactions Over Time") +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), 
        axis.title.x = element_text(size = 9), 
        axis.title.y = element_text(size = 9),
        plot.title = element_text(size = 12, face = "bold", margin = margin(0, 0, 20, 0)),
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm"))

# Filter the data for the month of December
transactions_december <- transactions_by_date %>%
  filter(month(DATE) == 12)

# Plot the number of transactions for December
ggplot(transactions_december, aes(x = DATE, y = Num_Transactions)) +
  geom_line() +
  labs(x = "Date", y = "Number of Transactions", title = "Number of Transactions in December") +
  scale_x_date(date_labels = "%d", date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 8),
        axis.text.y = element_text(size = 8), 
        axis.title.x = element_text(size = 9), 
        axis.title.y = element_text(size = 9),
        plot.title = element_text(size = 12, face = "bold", margin = margin(0, 0, 20, 0)),
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")) 

#### Pack size

transactionData[, PACK_SIZE := parse_number(PROD_NAME)]

#### Check if the pack sizes look sensible 
transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]

#### Plot a histogram of PACK_SIZE 
 
ggplot(transactionData, aes(x = PACK_SIZE)) +
  geom_histogram(fill = "#0E477F", color = "#EBEBEB") +
  labs(x = "PACK_SIZE", y = "Frequency", title = "Histogram of PACK_SIZE") +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), 
        axis.title.x = element_text(size = 9), 
        axis.title.y = element_text(size = 9),
        plot.title = element_text(size = 12, face = "bold", margin = margin(0, 0, 20, 0)),
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm"))

####  create Brands colomn
transactionData$BRAND <- sapply(strsplit(transactionData$PROD_NAME, " "), function(x) x[1])
print(unique(transactionData$BRAND))

# correcting the names of certain brands.
transactionData[BRAND == "Red", BRAND := "RRD"]
transactionData[BRAND == "Snbts", BRAND := "Sunbites"]
transactionData[BRAND == "NCC", BRAND := "Natural"]
transactionData[BRAND == "Infzns", BRAND := "Infuzions"]
transactionData[BRAND == "Dorito", BRAND := "Doritos"]
transactionData[BRAND == "Smith", BRAND := "Smiths"]
transactionData[BRAND == "WW", BRAND := "Woolworths"]
transactionData[BRAND == "Grain", BRAND := "GrnWves"]

print(unique(transactionData$BRAND))

#### Examining customer data

# Display the first few rows of the customerData dataset
str(customerData)

# Generate a summary of the customerData dataset
summary(customerData)
# the number of observations for customerData
num_observations <- nrow(transactionData)
print(num_observations)
# check for NA in the data
print(result <- sapply(customerData, is.null))

#### Examining the values of lifestage and premium_customer
customerData[, .N, by = LIFESTAGE][order(-N)]
customerData[, .N, by = PREMIUM_CUSTOMER][order(-N)]
# the number of unique loyalty card in transactionData
print(length(unique(transactionData$LYLTY_CARD_NBR)))
# the number of unique loyalty card in customerData
print(length(unique(customerData$LYLTY_CARD_NBR)))

data <- inner_join(transactionData, customerData, by = "LYLTY_CARD_NBR")

summary(data)
# Check for NA values in all columns of the data frame 
print(colSums(is.na(data)))
# Write the 'data' dataframe to a CSV file
fwrite(data, paste0("D:/DataProject/venv/Quantum/import","QVI_data.csv"))
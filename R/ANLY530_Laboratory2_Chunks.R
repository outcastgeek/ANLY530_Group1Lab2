## @knitr installLibraries

install.packages("knitr")
install.packages("kableExtra")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ModelMetrics")
install.packages("generics")
install.packages("gower")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("naivebayes")

## @knitr loadLibraries

library(dplyr)
library(ggplot2)
library(caret)
library(naivebayes)

## @knitr helperFunctions

# Obtains the full File Path
fullFilePath <- function(fileName)
{
  fileFolder <- "./data/"
  fileNamePath <- paste(fileFolder, fileName, sep = "")
  fileNamePath
}

# Converts column of Timestamps to Date
ttColToDate <- function(dFrame, colName) {
  dFrame[colName] <- as.POSIXct(dFrame[colName], origin="1970-01-01")
  dFrame
}

# Converts column to utf-8
toUtf8 <- function(column) {
  columnUtf8 <- iconv(enc2utf8(column), sub = "byte")
  columnUtf8
}

# Formats Data
fmt <- function(dt, caption = "") {
  fmt_dt <- dt %>%
    kable("latex", longtable = T, booktabs = T)
  fmt_dt
}

# Style Data
style <- function(dt, full_width = F, angle = 0) {
  style_dt <- dt %>%
    kable_styling(latex_options = "striped", full_width = full_width) %>%
    row_spec(0, angle = angle)
  style_dt
}

## @knitr loadSheets

#Set Data File Name:
creditDataFile <- "creditData.csv"
onlineNewsPopularityFile <- "OnlineNewsPopularity.csv"

# credit
credit <- creditDataFile %>%
  fullFilePath %>%
  read.csv(encoding = "UTF-8", header=TRUE, stringsAsFactors=FALSE)

# popularity
onlineNewsPopularity <- onlineNewsPopularityFile %>%
  fullFilePath %>%
  read.csv(encoding = "UTF-8", header=TRUE, stringsAsFactors=FALSE)

## @knitr part1Step1

sum(is.na(credit))

# Randomize the data
credit_rand <-credit[order(runif(1000)), ]

# Scale the data
creditDataScaled <- scale(credit_rand[,2:ncol(credit_rand)], center=TRUE, scale = TRUE)

# Compute the correlation matrix
m <- cor(creditDataScaled)

# Determine the threshold to use for feature (variable) selection and perform feature selection
highlycor <- findCorrelation(m, 0.30)

# Recombine the class variable with the highly correlated credit data and split into training and test data sets
filteredData <- credit_rand[, -(highlycor[5]+1)]
filteredTraining <- filteredData[1:750, ]
filteredTest <- filteredData[751:1000, ]

## @knitr part1Step2

# Build and evaluate the Naive Bayes Classifier as usual
filteredTraining$Creditability <- filteredTraining$Creditability %>% as.factor()
nb_model <- naive_bayes(Creditability ~ ., data = filteredTraining)


## @knitr part1Step3

# Evaluate the Naive Bayes Classifier as usual
filteredTestPred <- predict(nb_model, newdata = filteredTest)
table(filteredTestPred, filteredTest$Creditability)

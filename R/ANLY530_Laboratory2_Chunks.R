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

sum(is.na(credit))
str(credit)

# Pre-Processing
credit$Creditability <- credit$Creditability %>% as.factor()
credit %>% is.na() %>% sum()

# Set seed for Random
set.seed(12345)

# Randomize data
credit_rand <- credit[order(runif(1000)), ]

## @knitr part1Step1

# 75% means 750 for training and the rest for testing
credit_train <- credit_rand[1:750, ]
credit_test <- credit_rand[751:1000, ]

prop.table(table(credit_train$Creditability))
prop.table(table(credit_test$Creditability))

## @knitr part1Step2

naive_model <- naive_bayes(Creditability ~ ., data= credit_train)
naive_model

## @knitr part1Step3

(conf_nat <- table(predict(naive_model, credit_test), credit_test$Creditability))
(Accuracy <- sum(diag(conf_nat))/sum(conf_nat)*100)

## @knitr part2Step1

# Randomize the data
credit_rand <-credit[order(runif(1000)), ]

# Scale the data
creditDataScaled <- scale(credit_rand[,2:ncol(credit_rand)], center=TRUE, scale = TRUE)

# Compute the correlation matrix
m <- cor(creditDataScaled)

# Determine the threshold to use for feature (variable) selection and perform feature selection
highlycor <- findCorrelation(m, 0.30)

# Recombine the class variable with the highly correlated credit data and split into training and test data sets
filteredData <- credit_rand[, -(highlycor[2]+1)]
filteredTraining <- filteredData[1:750, ]
filteredTest <- filteredData[751:1000, ]

## @knitr part2Step2

# Build and evaluate the Naive Bayes Classifier as usual
nb_model <- naive_bayes(Creditability ~ ., data = filteredTraining)


## @knitr part2Step3

# Evaluate the Naive Bayes Classifier as usual
filteredTestPred <- predict(nb_model, newdata = filteredTest)
table(filteredTestPred, filteredTest$Creditability)

(conf_nat <- table(filteredTestPred, filteredTest$Creditability))
(Accuracy <- sum(diag(conf_nat))/sum(conf_nat)*100)

## @knitr part3Step1

# Data Preperation
newsShort <- data.frame(
  onlineNewsPopularity$n_tokens_title,
  onlineNewsPopularity$n_tokens_content,
  onlineNewsPopularity$n_unique_tokens,
  onlineNewsPopularity$n_non_stop_words,
  onlineNewsPopularity$num_hrefs,
  onlineNewsPopularity$num_imgs,
  onlineNewsPopularity$num_videos,
  onlineNewsPopularity$average_token_length,
  onlineNewsPopularity$num_keywords,
  onlineNewsPopularity$kw_max_max,
  onlineNewsPopularity$global_sentiment_polarity,
  onlineNewsPopularity$avg_positive_polarity,
  onlineNewsPopularity$title_subjectivity,
  onlineNewsPopularity$title_sentiment_polarity,
  onlineNewsPopularity$abs_title_subjectivity,
  onlineNewsPopularity$abs_title_sentiment_polarity,
  onlineNewsPopularity$shares
  )

colnames(newsShort) <- c(
  "n_tokens_title",
  "n_tokens_content",
  "n_unique_tokens",
  "n_non_stop_words",
  "num_hrefs",
  "num_imgs",
  "num_videos",
  "average_token_length",
  "num_keywords",
  "kw_max_max",
  "global_sentiment_polarity",
  "avg_positive_polarity",
  "title_subjectivity",
  "title_sentiment_polarity",
  "abs_title_subjectivity",
  "abs_title_sentiment_polarity",
  "shares"
  )

# Data Preperation
newsShort$popular = rep('na', nrow(newsShort))
for(i in 1:39644) {
  if(newsShort$shares[i] >= 1400) {
    newsShort$popular[i] = "yes"} 
  else {newsShort$popular[i] = "no"}
}
newsShort$shares = newsShort$popular
newsShort$shares <- as.factor(newsShort$shares)

# Data Preperation
news_rand <- newsShort[order(runif(10000)), ]

# Split the data into training and test datasets
news_train <- news_rand[1:9000, ]
news_test <- news_rand[9001:10000, ]


## @knitr part3Step2

nb_model <- naive_bayes(shares ~ ., data=news_train)
nb_model

## @knitr part3Step3

news_Pred <- predict(nb_model, newdata = news_test)
(conf_nat <- table(news_Pred, news_test$shares))

(Accuracy <- sum(diag(conf_nat))/sum(conf_nat)*100)


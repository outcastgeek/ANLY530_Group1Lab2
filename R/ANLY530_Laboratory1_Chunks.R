## @knitr installLibraries

install.packages("knitr")
install.packages("kableExtra")
install.packages("dplyr")
install.packages("C50")
install.packages("gmodels")
install.packages("kernlab")
install.packages("rpart")
install.packages("rpart.plot")

## @knitr loadLibraries

library(dplyr)
library(C50)
library(gmodels)
library(kernlab)
library(rpart)
library(rpart.plot)

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
credit_DataFile <- "credit.csv"
letters_DataFile <- "letterdata.csv"
wine_DataFile <- "whitewines.csv"

# credit
credit <- credit_DataFile %>%
  fullFilePath %>%
  read.csv(encoding = "UTF-8", header=TRUE, stringsAsFactors=FALSE)

# letters
letters <- letters_DataFile %>%
  fullFilePath %>%
  read.csv(encoding = "UTF-8", header=TRUE, stringsAsFactors=FALSE)

# wine
wine <- wine_DataFile %>%
  fullFilePath %>%
  read.csv(encoding = "UTF-8", header=TRUE, stringsAsFactors=FALSE)

## @knitr method1Step1CollectingData

str(credit)
summary(credit$amount)
table(credit$default)
set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]

## @knitr method1Step2ExploringData

# Check no change after randomizing
summary(credit$amount)
summary(credit_rand$amount)

# Training and Testing Sets split

credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]

# Check percentages

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## @knitr method1Step3ModelTraining

# Traing a model on the data
credit_model <- C5.0.default(x = credit_train[-17], y = credit_train$default %>% as.factor())
summary(credit_model)

## @knitr method1PerformanceEvaluation

# Evaluating Model Performance

cred_pred <- predict(credit_model, credit_test)

## @knitr method1ConfusionTable

CrossTable(credit_test$default, cred_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## @knitr method2Step1CollectingData

str(letters)

## @knitr method2Step2PreparinggData

letters_train <- letters[1:18000, ]
letters_test <- letters[18001:20000, ]

## @knitr method2Step3ModelTraining

letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")
letter_classifier

## @knitr method2PerformanceEvaluation

letter_predictions <- predict(letter_classifier, letters_test)
table(letter_predictions, letters_test$letter)

## @knitr method2AgreementTable

agreement <- letter_predictions == letters_test$letter
table(agreement)

## @knitr method3Step1CollectingData

hist(wine$quality)

## @knitr method3Step2ExploringPreparingData

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

## @knitr method3Step3ModelTraining

m.rpart <- rpart(quality ~ ., data=wine_train)
m.rpart

par(mfrow=c(2,1)) # 2 plots in two rows and one column
rpart.plot(m.rpart, digits=3)
rpart.plot(m.rpart, digits=4, fallen.leaves = TRUE, type = 3, extra = 101)

## @knitr method3Step4PerformanceEvaluation

p.rpart <- predict(m.rpart, data=wine_test)
p.rpart

summary(p.rpart)
summary(wine_test$quality)
#cor(p.rpart, wine_test$quality)

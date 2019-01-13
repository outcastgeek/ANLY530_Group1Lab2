## @knitr installLibraries

install.packages("knitr")
install.packages("kableExtra")
install.packages("dplyr")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("naivebayes")

## @knitr loadLibraries

library(dplyr)
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
letters <- onlineNewsPopularityFile %>%
  fullFilePath %>%
  read.csv(encoding = "UTF-8", header=TRUE, stringsAsFactors=FALSE)

## @knitr Part1

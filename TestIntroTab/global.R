# libraries
library(shiny)
library(jsonlite)
library(shinyjs)
library(MASS)
library(readr)
library(ggplot2)

# create help data frame
steps <- read.csv("help.csv")

# 使用するデータの用意
score <- read.csv("/path/to/data.csv", header = TRUE)

# データの準備 : テストデータは1,2行目
score.train <- score[-1:-2,]
score.test <- score[1:2,]

# -----  ログ記録用 関数 ----------
save_logData <- function(username, what_user_did, changed_value1, changed_value2) {
  date <- format(Sys.time(), "%Y/%m/%d")
  time <- format(Sys.time(), "%H:%M:%OS")
  x <- matrix(c(username, date, time, what_user_did, changed_value1, changed_value2),
              nrow = 1, ncol = 6)
  write.table(x, file = "test.csv", append = TRUE, sep = ',',
              row.names = FALSE, col.names = FALSE)
}

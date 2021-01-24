# libraries
library(shiny)
library(shinyjs)
library(jsonlite)
library(MASS)
library(readr)
library(ggplot2)
library(shinythemes)

#ファイル名に時刻を入れる
# today <- Sys.Date() #日付取得
# now <- Sys.time() # 時刻取得
# ti <- format(now, "%H-%M-%S")

# -----  データ読み込み  -----
d.new <- read.csv("/path/to/data.csv", header = TRUE)
steps <- read.csv("help.csv")


# --- 新しい特徴量 ---
x <- d.new[, c(6,7)]
y <- d.new[, -c(6,7)]

x <- apply(x, 2, function(x) {(x - mean(x)) / sd(x)})
music <- cbind(y, x)
music <- as.data.frame(music)
# ---

# データの準備 : 
music.train <- music[c(1:568),]
music.test <- music[c(569:611),]

# ログ記録用 関数 ----------
save_logData <- function(username, what_user_did, changed_value1, changed_value2) {
  if (!(is.null(username))){
    date <- format(Sys.time(), "%Y/%m/%d")
    time <- format(Sys.time(), "%H:%M:%OS")
    
    file.name <- paste("/path/to/save/files/", username, ".csv", sep = "")
    x <- matrix(c(username, date, time, what_user_did, changed_value1, changed_value2),
                nrow = 1, ncol = 6)
    write.table(x, file = file.name, append = TRUE, sep = ',',
                row.names = FALSE, col.names = FALSE, quote=FALSE)
  }
}


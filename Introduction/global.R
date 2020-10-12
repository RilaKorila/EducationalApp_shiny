library(shiny)
library(ggplot2)
library(MASS)

d.new <- read.csv("path/to/data", header = TRUE)

# --- 新しい特徴量 ---
x <- d.new[, c(6,7)]
y <- d.new[, -c(6,7)]

x <- apply(x, 2, function(x) {(x - mean(x)) / sd(x)})
music <- cbind(y, x)
music <- as.data.frame(music)

# データの準備 : テストデータは1,2行目
num <- c(seq(1, 250, by = 8), seq(450, nrow(music), by = 6))
music.train <- music[-num,]
music.test <- music[num,]

library(shiny)
library(readr)
library(ggplot2)
library(MASS)
library(shinyjs)

d.new <- read.csv("/path/to/data.csv", header = TRUE)

# --- 新しい特徴量 ---
x <- d.new[, c(6,7)]
y <- d.new[, -c(6,7)]

x <- apply(x, 2, function(x) {(x - mean(x)) / sd(x)})
music <- cbind(y, x)
music.train <- as.data.frame(music)

# --- データの準備---
music.train <- music[c(1:568),]
music.test <- music[c(569:611),]


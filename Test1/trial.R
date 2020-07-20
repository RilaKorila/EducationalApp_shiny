library(readr)
library(MASS)
library(ggplot2)
library(plotly)
score <- read.csv("/path/to/data.csv")


# データの準備 : テストデータは1,2行目
score.train <- score[-1:-2,]
score.test <- score[1:2,]

# ctr+shift+c でまとめてコメントアウト
# x.train <- score.train[, "math"]
# y.train <- score.train[, "english"]
# x.test <- score.test[, "math"]
# y.test <- score.test[, "english"]
# rank.test <- score.test$rank

score.train <-  cbind(score.train[, "rank"],
                      score.train[, "math"],
                      score.train[, "japanese"])
score.train <- as.data.frame(score.train)


score.test <-  cbind(score.test[, "rank"],
                     score.test[, "math"],
                     score.test[, "japanese"])
score.test <- as.data.frame(score.test)

res.lda <- lda(V1 ~ V2 + V3, data=score.train)
pre <- predict(res.lda, score.test)$class

t <- table(pre, score.test$V1)

# パッケージplotlyを使ってみる
g <- ggplot(score, aes(math, english, color = rank)) +
  geom_point() 
ggplotly(g)

# - - - 3dのplotについて - - - 
plot3d(score)
plot3d(score$math, score$english, score$japanese, col = "red")
plot3d(score$math, score$english, score$japanese, col = as.integer(score$rank))
# WebGLに保存するとHTML形式で結果のみ表示できるらしい
 # writeWebGL(width=500, height=550)

# - - - データの抽出 - - -
logi <- c(FALSE, TRUE, FALSE, FALSE)
logi <- matrix(logi, nrow = 1, ncol = 4)
x <- c("a", "b", "c", "d")
x <- matrix(x, nrow = 1, ncol = 4)
x[logi == TRUE]


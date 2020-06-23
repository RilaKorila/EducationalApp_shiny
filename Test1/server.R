#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(readr)
library(ggplot2)
score <- read.csv("/Users/ayana/shiny/Test1/data/shiny_test.csv", header = TRUE)

# Define server logic required to draw a histogram
shinyServer(
    function(input, output) {
    # renderPlot内のコードは入力変更のたびに再実行 
        # + 返り値はPlot
   
    # 散布図を描画する関数
    # 中の引数に変更がある時以外はキャッシュした値を返す
    drawPlot <- reactive({
        
        # input$newplot
        # Add a little noise to the cars data
        x <- score[, input$xlabel]
        y <- score[, input$ylabel]
        # plot(x, y, xlim = c(0,100), ylim = c(0, 100),
          #   col = c("red", "#58FAD0"))
        ggplot(score, aes(x, y,alpha = 1.0, color = rank)) + geom_point() 
        
    })
    
    drawPlot2 <- reactive({ # Exculsive対応
        # Plot the kept and excluded points as two separate data sets
        keep    <- score[ vals$keeprows, , drop = FALSE]
        exclude <- score[!vals$keeprows, , drop = FALSE]
        
        ggplot(keep, aes(xval(), yval(), color = rank)) + 
            geom_point() +
            geom_point(data = exclude, shape = 21, fill = NA, color = "white", alpha = 0.25)
    })
    
    # Toggle points that are clicked (DeleteボタンがTrueのときのみ)
    observeEvent(input$plot_click, {
        if (input$deleteMode) {
            res <- nearPoints(score, input$plot_click, allRows = TRUE)
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        }
    })
    
    # 線形判別の計算
    # 返り値：線形判別後の分類精度accuracy
    ldaExe <- reactive({
        # データの準備 : テストデータは1,2行目
        score.train <- score[-1:-2,]
        score.test <- score[1:2,]
        #作成した変数名x.nameにinput$xlabelを格納
        x.name <- input$xlabel
        y.name <- input$ylabel
        
        score.train <-  cbind(score.train[, "rank"],
                              score.train[, x.name],
                              score.train[, y.name])
        score.train <- as.data.frame(score.train)
        
        
        score.test <-  cbind(score.test[, "rank"],
                             score.test[, x.name],
                             score.test[, y.name])
        score.test <- as.data.frame(score.test)
        # 線形判別
        lda.model <- lda(V1 ~ V2 + V3, data=score.train)
        pre <- predict(lda.model, score.test)$class
        
        tbl <- table(pre, score.test$V1)
        # lda: 同じ変数を指定するとWarning..variables are collinea発生r
        
        sum <- sum(rowSums(tbl))
        correct <- tbl[1,1] + tbl[2,2]
        accuracy <- (correct / sum) * 100
    })
    
    # ActionButtonが押されて初めてboundaryのデータが更新される
    boundaryData <- eventReactive(input$boundary, {
        a <- input$intercept
        b <- input$slope
        a.b <- c(a, b)
        a.b
    })
    
    output$plot <- renderPlot({
        if (input$boundary) {
            boundaryData.a.b <- boundaryData()
            drawPlot() + 
                geom_abline(intercept = boundaryData.a.b[1], slope = boundaryData.a.b[2])
        }
       else{
           p <- drawPlot()
           ldaExe() # pを表示する前に入れないとggplotが消える
           p
       }
    })
    
    # return Name of the x, y
    xvar <- reactive({
        switch(input$xlabel, math = "math", english = "english", japanese = "japanese")
    })
    yvar <- reactive({
        switch(input$ylabel, math = "math", english = "english", japanese = "japanese")
    })
    
    output$click_info <- renderPrint({
        # Select just the nearest point within 10 pixels of the click
        res <- nearPoints(score, input$plot_click, xvar(), yvar(), threshold = 10, maxpoints = 1)
        res
    })
    
    output$accuracy <- renderText({
        acc <-  ldaExe()
    })
    
    # For storing which rows have been excluded(排除された行を記録)
    values <- reactiveValues(
        # もしかしてExcludeできるのはscore.trainの方だけ？
        keeprows = rep(TRUE, nrow(score))
    ) 

    
})

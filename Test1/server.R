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
    
    # CheckBoxにチェックしたまま変更すると値がNULLになる、、
    boundaryData <- reactive({
        a <- input$intercept
        b <- input$slope
        a.b <- c(a, b)
        a.b
    })
    
    output$plot <- renderPlot({
        if (input$boundary) {
            boundaryData.a.b <- boundaryData()
            drawPlot() # + 直線
            
            # abline(boundaryData.a.b[1], boundaryData.a.b[2])
               # geom_abline(intercept = boundaryData.a.b[1],
                #            slope = boundaryData.a.b[2])
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
        # 出力をどうにかしないといけない
        #spaste("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })
    
    output$accuracy <- renderText({
        acc <-  ldaExe()
    })
    
    
    output$xlabel_get <- renderPrint({
        paste ("you have selected ", input$xlabel)
    })
    output$ylabel_get <- renderPrint({
        paste ("you have selected ", input$ylabel)
    })

})

shinyServer(function(input, output, session) {
  
  # ----- reactive関数：Plot描画  -----
  drawPlot2 <- reactive({ 
    ggplot(music.train, aes(umap_x, umap_y, color = label, size = 5, alpha = 0.5)) +
      geom_point() +
      scale_alpha_continuous(guide = FALSE) + 
      scale_size_continuous(guide = FALSE) 
    
  })
  
  # ----- reactive関数：テストデータを分類  -----
  ldaExe <- reactive({
    # 線形判別: 自作の境界線により判断
    lda.model <- lda(label ~ umap_x + umap_y, data = music.train)
    pre <- predict(lda.model, music.test)$class
    tbl <- table(music.test$label, pre)
  })
  
  # ----- reactive関数：境界線描画  -----
  addBoundary <- reactive({
    xmin <- round(input$plot_brush$xmin, digits = 2)
    ymin <- round(input$plot_brush$ymin, digits = 2)
    xmax <- round(input$plot_brush$xmax, digits = 2)
    ymax <- round(input$plot_brush$ymax, digits = 2)
    #isolate入れないと！clickだけの時エラーになってしまう
    isolate({
      start.x <- round(input$plot_click$x, digits = 2)
      start.y <- round(input$plot_click$y, digits = 2)
    })
    
    
    if (xmin == start.x){
      start.x <- xmin
      end.x <- xmax
    }
    else{
      start.x <- xmax
      end.x <- xmin
    }
    # y座標の始点と終点を取得
    if (ymin == start.y){
      start.y <- ymin
      end.y <- ymax
    }
    else{
      start.y <- ymax
      end.y <- ymin
    }
    
    # 精度表示ボタンのロックを解除
    enable("calAccuracy")
    enable("plotTest")
    
    if (start.x == end.x){
      # x = start.xの直線
      # g <- drawPlot2()
      # g + geom_vline(aes(xintercept = start.x), linetype="dashed")
      geom_vline(aes(xintercept = start.x), linetype="dashed")
    }
    else if(start.y == end.y){
      # y = start.yの直線
      # g <- drawPlot2()
      # g + geom_hline(aes(yintercept = start.y), linetype="dashed")
      geom_hline(aes(yintercept = start.y), linetype="dashed")
    }
    else{ #斜めの直線 slopeとinterceptを求める
      cat("else")
      a <- (end.y - start.y) / (end.x - start.x)
      b <- start.y - a * start.x

      geom_abline(aes(intercept = b, slope = a), linetype="dashed")
    }
  })
  
  # ----- reactive関数：テストデータを描画  -----
  addTestPlot <- reactive({
    g <- ggplot(music.train, aes(umap_x, umap_y, color = label, size = 5)) +
      geom_point(alpha = 0.12) +
      scale_alpha_continuous(guide = FALSE) + 
      scale_size_continuous(guide = FALSE) 
    g <- g + geom_point(music.test, mapping = aes(umap_x, umap_y), shape = 8, alpha = 1)
    g
    })
  
  # ----- reactive関数：判別精度テーブルを取得  -----
  getAccuracy <- reactive({
    #tbl <-  isolate(ldaExe())
    # 予測ラベル　青
    red.red <- nrow(subset(music.test, (label == "after") & (new.label == "red")))
    # 予測ラベル　赤
    blue.red <- nrow(subset(music.test, (label == "before") & (new.label == "red")))
    # 正解ラベル 青
    red.blue <- nrow(subset(music.test, (label == "after") & (new.label == "blue")))
    # 正解ラベル 赤
    blue.blue <- nrow(subset(music.test, (label == "before") & (new.label == "blue")))
    
    count <- c(red.red, blue.red, red.blue, blue.blue)
    tbl <- as.data.frame(count)
    tbl
  })
  
  # ----- render関数：%形式で精度表示  -----
  output$accuracy <- renderText({ 
    if (is.null(input$plot_brush$xmin)){
      # brushする前は何も表示しない
    }
    else{
      if(input$calAccuracy == 0){
        # まだボタンが押されていない時
      }
      else{
        acc <- isolate(getAccuracy())
        sum <- acc[1,3] + acc[2,3] + acc[3,3] + acc[4,3]
        correct <- acc[1,3] + acc[4,3]
        accuracy <- (correct / sum) * 100
        accuracy <- round(accuracy, digits = 1)
        paste("精度: " ,accuracy, " %")
      }
    }
  })
  
  # ----- render関数：一番シンプルな訓練データ描画  -----
  output$plot <- renderPlot({
    drawPlot2()
  })
  
  
  # ----- render関数：境界線つきの散布図描画  -----
  output$plot_output <-renderPlot({
    if (is.null(input$plot_brush$xmin)){
      cat("1")
    }
    else if ((input$plotTest == TRUE) & (is.null(input$plot_brush$xmin) == FALSE)) {
      # テストデータ表示
      cat("2")
      g <- addTestPlot()
      g <- g + addBoundary()
      g
    }
    else if ((input$plotTest == FALSE) & (is.null(input$plot_brush$xmin) == FALSE)){
      cat("3")
      g <- drawPlot2()
      g <- g + addBoundary()
      g
    }
  })
})
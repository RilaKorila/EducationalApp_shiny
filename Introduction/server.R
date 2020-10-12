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
    
    if (start.x == end.x){
      # x = start.xの直線
      g <- drawPlot2()
      g + geom_vline(aes(xintercept = start.x), linetype="dashed")
    }
    else if(start.y == end.y){
      # y = start.yの直線
      g <- drawPlot2()
      g + geom_hline(aes(yintercept = start.y), linetype="dashed")
    }
    else{ #斜めの直線 slopeとinterceptを求める
      a <- (end.y - start.y) / (end.x - start.x)
      b <- start.y - a * start.x
      #g <- drawPlot2()
      #g + geom_abline(aes(intercept = b, slope = a), linetype="dashed")
      geom_abline(aes(intercept = b, slope = a), linetype="dashed")
    }
  })
  
  # ----- reactive関数：テストデータを描画  -----
  addTestPlot <- reactive({
    x.toPlot <- music.test[, "umap_x"]
    y.toPlot <- music.test[, "umap_y"]
    #layer(aes(x.toPlot, y.toPlot, color = label, size = 5, alpha = 5))
    geom_point(music.test, mapping = aes(umap_x, umap_y), shape = 8, alpha = 1)
    })
  
  # ----- reactive関数：判別精度テーブルを取得  -----
  getAccuracy <- reactive({
    cat("get accuracy\n")
    tbl <-  isolate(ldaExe())
    tbl <- as.data.frame(tbl)
    colnames(tbl) <- c("正解ラベル","予測ラベル", "データ数")
    tbl
  })
  
  
  # ----- render関数：テーブル形式で精度表示  -----
  output$accuracyTable <- renderTable({ 
    if (is.null(input$plot_brush$xmin)){
      cat("before brush\n")
      # brushする前は何も表示しない
    }
    else{
      cat("after brush\n")
      if(input$calAccuracy == 0){
        # まだボタンが押されていない時
      }
      else{
        acc <- isolate(getAccuracy())
       # ラベルことの件数をカウント
        acc <- rbind(acc11, acc12, acc13, acc14)
        acc <- as.data.frame(acc)
        colnames(acc) <- c("正解ラベル","予測ラベル", "件数")
        acc
      }
    }
  })
  
  # ----- render関数：%形式で精度表示  -----
  output$accuracy <- renderText({ 
    if (is.null(input$plot_brush$xmin)){
      cat("before brush\n")
      # brushする前は何も表示しない
    }
    else{
      cat("after brush\n")
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
    g <- drawPlot2()
    if (is.null(input$plot_brush$xmin)){
      # brushする前は何も表示しない
      # g <- drawPlot2()
      # g
    }
    else{
      g <- g + addBoundary()
      
      if (input$plotTest == TRUE){
        # テストデータ表示
        g <- g + addTestPlot()
      }
      g
    }
  })
  
  # output$click_info <- renderPrint({
  #   start.x <- round(input$plot_click$x, digits = 2)
  #   start.y <- round(input$plot_click$y, digits = 2)
  #   paste(start.x, "&", start.y)
  # })
  
  
  
})
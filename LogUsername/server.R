shinyServer(function(input, output, session) {
  
  # set help content
  session$sendCustomMessage(type = 'setHelpContent', message = list(steps = toJSON(steps) ))
  
  # listen to the action button
  observeEvent(input$startHelp,{
    # on click, send custom message to start help
    session$sendCustomMessage(type = 'startHelp', message = list(""))
  })
  
  # ------  ログ取得用  -------
  # name.col.list <- c("name", "date", "time", "action", "value1", "value2")
  # df <- data.frame(matrix(rep(NA, length(name.col.list)), nrow=1))[numeric(0), ]
  # colnames(df) <- name.col.list
  # x <- as.data.frame(df)
  # write.table(x, file = file.name, sep = ",")
  # ----------------------------
  
  # ユーザー名を取得
  getUserName <- reactive({
    # 登録ボタンが押された時のみ名前変更
    input$registerBtn
    # それ以外のusernameの変化はisolate
    new.name <- isolate(input$username)
  
    new.name
  })
  
  observeEvent(input$registerBtn,{
    user.name <- input$username
    # ----- ログ取得用  -----
    save_logData(user.name, "register a username", user.name, "")
    # -----------------------
    
    output$message <- renderText("ユーザー名が登録できました！")

    # 操作盤のロック解除
    enable("xlabel")
    enable("ylabel")

    # 名前の変更をロック
    disable("username")
    disable("registerBtn")
  })
  
  # ----- reactive関数：Plot用のデータ用意  -----
  prepareData <- reactive({ # testデータ含まない
    # Plot the kept and excluded points as two separate data sets
    keep    <- music.train[ vals$keeprows, , drop = FALSE]
    exclude <- music.train[!vals$keeprows, , drop = FALSE]
    
    data.toPlot <- rbind(keep, exclude)
    data.type <- c(rep("keep", NROW(keep)), rep("exclude", NROW(exclude)))
    data.toPlot <- cbind(data.toPlot, data.type)
  })
  
  prepareDataIncludeTest <- reactive({ # testデータ含む
    # Plot the kept and excluded points as two separate data sets
    keep    <- music.train[ vals$keeprows, , drop = FALSE]
    exclude <- music.train[!vals$keeprows, , drop = FALSE]
    
    data.toPlot <- rbind(keep, music.test, exclude)
    data.type <- c(rep("keep", NROW(keep)), rep("test", NROW(music.test)), rep("exclude", NROW(exclude)))
    data.toPlot <- cbind(data.toPlot, data.type)
  })
  
  # ----- reactive関数：Plot描画  -----
  drawPlot2 <- reactive({ 
    # Plot the kept and excluded points as two separate data sets
    keep    <- music.train[ vals$keeprows, , drop = FALSE]
    exclude <- music.train[!vals$keeprows, , drop = FALSE]
    
    x.label <- xvar()
    y.label <- yvar()
    
    data.toPlot <- prepareData()
    
    x.toPlot <- data.toPlot[, x.label]
    y.toPlot <- data.toPlot[, y.label]
 
    # --- lda再実行(境界線表示用) ---
    lda.train <-  cbind(V1 = keep[, "label"],
                        V2 = keep[, x.label],
                        V3 = keep[, y.label])
    lda.train <- as.data.frame(lda.train)
    
    lda.test <-  cbind(V1 = music.test[, "label"],
                       V2 = music.test[, x.label],
                       V3 = music.test[, y.label])
    lda.test <- as.data.frame(lda.test)
    
    res.lda <- lda(V1 ~ V2 + V3, data = lda.train)
    pre <- predict(res.lda, lda.test)$class
    # ----------
    
    # --- ldaの結果から境界線のslopeとinterceptを算出---
    res.slope = -1 * res.lda$scaling[1] / res.lda$scaling[2]
    res.intercept = apply(res.lda$means%*%res.lda$scaling, 2, mean) / res.lda$scaling[2]
    # -----
    
    # ----- ログ取得用  -----
    save_logData(getUserName(), "current labels", x.label, y.label)
    # -----------------------
    
    cols <- c("keep" = 16, "exclude" = 1)
    ggplot(data.toPlot, aes(x.toPlot, y.toPlot, color = label, shape = data.type, size = 5, alpha = 0.5)) +
      geom_point() +
      geom_abline(aes(intercept = res.intercept , slope = res.slope), linetype="dashed") + # 境界線
      labs( x = x.label, y = y.label ) +                                # 軸のラベル
      # 凡例の表記
      theme_grey(base_family = "HiraKakuProN-W3") +                     # Font
      #scale_shape_manual(values=cols, name = "データ種類", labels = c(keep = "訓練データ　", exclude = "削除済データ")) +
      scale_shape_manual(values=cols, guide = FALSE) +
      scale_color_hue(name = "年代", labels = c(before = "1999年以前", after ="2000年以降") ) +
      scale_alpha_continuous(guide = FALSE) + 
      scale_size_continuous(guide = FALSE)
  })
  
  # ----- reactive関数：TestPlot描画  -----
  drawTestPlot2 <- reactive({ # Exculsive対応
    # Plot the kept and excluded points as two separate data sets
    keep    <- music.train[ vals$keeprows, , drop = FALSE]
    exclude <- music.train[!vals$keeprows, , drop = FALSE]
    
    x.label <- xvar()
    y.label <- yvar()
    
    #data.toPlot <- prepareDataIncludeTest()
    data.toPlot <-  rbind(keep, music.test, exclude)
    data.type <- c(rep("keep", NROW(keep)), rep("test", NROW(music.test)), rep("exclude", NROW(exclude)))
    data.toPlot <- cbind(data.toPlot, data.type)
    x.toPlot <- data.toPlot[, x.label]
    y.toPlot <- data.toPlot[, y.label]
    
    # --- lda再実行(テストデータの色分け用) ---
    lda.train <-  cbind(V1 = keep[, "label"],
                        V2 = keep[, x.label],
                        V3 = keep[, y.label])
    lda.train <- as.data.frame(lda.train)

    lda.test <-  cbind(V1 = music.test[, "label"],
                       V2 = music.test[, x.label],
                       V3 = music.test[, y.label])
    lda.test <- as.data.frame(lda.test)

    res.lda <- lda(V1 ~ V2 + V3, data = lda.train)
    pre <- predict(res.lda, lda.test)$class
    # ----------
    
    # --- ldaの結果から境界線のslopeとinterceptを算出---
    res.slope = -1 * res.lda$scaling[1] / res.lda$scaling[2]
    res.intercept = apply(res.lda$means%*%res.lda$scaling, 2, mean) / res.lda$scaling[2]
    # -----
    
    # --- music.testのlabelを判別分析結果に変更  ---
    pre.vector <- gsub("1", "after", pre)
    pre.vector <- gsub("2", "before", pre.vector)
    music.test$label <- pre.vector
    # -----
    
    cols <- c("keep" = 16, "test" = 8, "exclude" = 1)
    ggplot(data.toPlot, aes(x.toPlot, y.toPlot, color = label, shape = data.type, size = 5, alpha = 0.5)) +
      geom_point() +
      geom_abline(aes(intercept = res.intercept , slope = res.slope), linetype="dashed") + # 境界線
      labs( x = x.label, y = y.label ) +                                # 軸のラベル
      # 凡例の表記
      theme_grey(base_family = "HiraKakuProN-W3") +                     # Font
      scale_color_hue(name = "色分け", labels = c(before = "1999年以前", after ="2000年以降") ) +
      scale_shape_manual(values=cols, name = "データ種類", labels = c(keep = "訓練データ", exclude = "排除済データ",test = "テストデータ", after ="2000年以降")) +
      scale_alpha_continuous(guide = FALSE) + 
      scale_size_continuous(guide = FALSE)
  })
  
  # -----  削除したデータのカウント  -----
  output$deleteNum <- renderText({
    keep <- music.train[ !vals$keeprows, , drop = FALSE]
    paste("削除したデータ数：　　", nrow(keep), " 個")
  })
  
  # -----  訓練データの削除・参照  -----
  # clickした点を削除
  observeEvent(input$plot_click, {
    res <- nearPoints(music.train, input$plot_click, xvar(), yvar(), allRows = TRUE, maxpoints = 1)
    # ----- ログ取得用  -----
    if(is.element(TRUE, res$selected)){
      save_logData(getUserName(), "delete a record",  as.character(res[res$selected_ == TRUE, 3]),  as.character(res[res$selected_ == TRUE, 5]))
    }
    # -----------------------
    vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
  # hoverした点の詳細情報を表示
  output$click_info <- renderTable({
    res <- nearPoints(music, input$plot_hover, xvar(), yvar(), threshold = 10, maxpoints = 1)
    
    # 出版年, 歌手名, 楽曲名のみ表示
    # ----- ログ取得用  -----
    if(nrow(res) == 1){
      save_logData(getUserName(), "reference a record",  as.character(res[1,3]),  as.character(res[1,5]))
    }
    # -----------------------
    res[c(1, 2, 3, 5)]
  }, bordered = TRUE)
  
  # -----  線形判別の計算  -----
  # 返り値2: table
  ldaExe <- reactive( {
    keep    <- music.train[ vals$keeprows, , drop = FALSE]
    exclude <- music.train[!vals$keeprows, , drop = FALSE]
    
    x.keep <- keep[, xvar()]
    y.keep <- keep[, yvar()]
    x.exclude <- exclude[, xvar()]
    y.exclude <- exclude[, yvar()]
    
    
    #作成した変数名x.nameにinput$xlabelを格納
    x.name <- input$xlabel
    y.name <- input$ylabel
    
    music.train <-  cbind(keep[, "label"],
                          keep[, x.name],
                          keep[, y.name])
    music.train <- as.data.frame(music.train)
    
    
    music.test <-  cbind(music.test[, "label"],
                         music.test[, x.name],
                         music.test[, y.name])
    music.test <- as.data.frame(music.test)
    # 線形判別
    lda.model <- lda(V1 ~ V2 + V3, data=music.train)
    pre <- predict(lda.model, music.test)$class
    
    tbl <- table(music.test$V1, pre)
  })
  
  # ----- グラフの描画 と 線形判別実行 -----
  output$plot <- renderPlot({
    p <- drawPlot2()
    ldaExe() # pを表示する前に入れないとggplotが消える
    p
  })
  
  # return Name of the x, y
  xvar <- reactive({
    switch(input$xlabel, 
           umap_x = "umap_x", umap_y = "umap_y")
  })
  yvar <- reactive({
    switch(input$ylabel, 
           umap_x = "umap_x", umap_y = "umap_y")
  })
  
  # For storing which rows have been excluded(排除された行を記録)
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(music.train))
  )
  
  
  # -----  随時 精度算出  -----
  showAccuracy <- reactive({
    #input$calAccuracy2
    #tbl <- isolate(ldaExe())
    tbl <- ldaExe()
    tbl <- as.data.frame(tbl)
    colnames(tbl) <- c("正解ラベル","予測ラベル", "データ数")
    tbl
  })
  
  # -----  ボタンが押されたときのみ精度算出  -----
  getAccuracy <- reactive({
    # ボタンが押された時のみ精度算出
    input$calAccuracy2
    # それ以外のusernameの変化はisolate
    tbl <- isolate(ldaExe())
    tbl <- as.data.frame(tbl)
    colnames(tbl) <- c("正解ラベル","予測ラベル", "データ数")
    tbl
  })
  
  # ----- 精度出力  -----
  output$accuracy <- renderTable({ 
    acc <- showAccuracy()
    # ----- ログ取得用  -----
    save_logData(getUserName(),"acc13_acc23",  acc[1,3], acc[2,3])
    save_logData(getUserName(),"acc33_acc43",  acc[3,3], acc[4,3])
    # -----------------------
    acc11 <- c("2000年以降", "2000年以降", acc[1,3])
    acc12 <- c("1999年以前", "2000年以降", acc[2,3])
    acc13 <- c("2000年以降", "1999年以前", acc[3,3])
    acc14 <- c("1999年以前", "1999年以前", acc[4,3])
    acc <- rbind(acc11, acc12, acc13, acc14)
    acc <- as.data.frame(acc)
    colnames(acc) <- c("正解ラベル","予測ラベル", "件数")
    acc
  })
  
  # --------------------------  以下Panel2用  -------------------------
  output$accuracy2 <- renderTable({ 
    acc <- showAccuracy()
    acc11 <- c("2000年以降", "2000年以降", acc[1,3])
    acc12 <- c("1999年以前", "2000年以降", acc[2,3])
    acc13 <- c("2000年以降", "1999年以前", acc[3,3])
    acc14 <- c("1999年以前", "1999年以前", acc[4,3])
    acc <- rbind(acc11, acc12, acc13, acc14)
    acc <- as.data.frame(acc)
    colnames(acc) <- c("正解ラベル","予測ラベル", "件数")
    acc
  })
  
  # -----  テストデータ用散布図情報  -----
  output$click_info2 <- renderTable({
    res <- nearPoints(music, input$plot_hover2, xvar(), yvar(), threshold = 10, maxpoints = 1)
    
    # ----- ログ取得用  -----
    if(nrow(res) == 1){
      save_logData(getUserName(), "TEST reference a record",  as.character(res[1,3]),  as.character(res[1,5]))
      #cat("hovered!")
    }
    # -----------------------
    # 出版年, 歌手名, 楽曲名のみ表示
    res[c(1, 2, 3, 5)]
  })
  
  # -----  テストデータの描画  -----
  output$test_plot <- renderPlot({
    drawTestPlot2()
  })
  
  # -----  削除されたデータの表示  -----
  output$deletedData <- renderTable({
    exclude <- music.train[ !vals$keeprows, , drop = FALSE]
    # ----- ログ取得用  -----
    save_logData(getUserName(), "Deleted Data Menu",  "データ数", nrow(exclude))
    # -----------------------
    exclude
  })
  
  
})
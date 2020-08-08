shinyServer(function(input, output, session) {
  
  # set help content
  session$sendCustomMessage(type = 'setHelpContent', message = list(steps = toJSON(steps) ))
  
  # listen to the action button
  observeEvent(input$startHelp,{
    # on click, send custom message to start help
    session$sendCustomMessage(type = 'startHelp', message = list(""))
  })
  
  # ------  ログ取得用  -------
  #username <- getUserName()
  filename <- "test.csv"
  name.col.list <- c("name", "date", "time", "action", "value1", "value2")
  df <- data.frame(matrix(rep(NA, length(name.col.list)), nrow=1))[numeric(0), ]
  colnames(df) <- name.col.list
  x <- as.data.frame(df)
  write.table(x, file = filename, sep = ",")
  # ----------------------------
  
  
  
  # -----  ユーザー名を取得  -----
  getUserName <- reactive({
    # 登録ボタンが押された時のみ名前変更
    input$registerBtn
    # それ以外のusernameの変化はisolate
    new.name <- isolate(input$username)
    new.name
  })
  
  # -----  usernameの登録  -----
  observeEvent(input$registerBtn,{
    user.name <- input$username
    # ----- ログ取得用  -----
    save_logData(user.name, "register a username", user.name, "")
    # -----------------------
    
    # 操作盤のロック解除
    enable("xlabel")
    enable("ylabel")
    enable("clickMode")
    enable("calAccuracy")
    # 名前の変更をロック
    disable("username")
    disable("registerBtn")
  })
  
  # ----- reactive関数：Plot描画  -----
  drawPlot2 <- reactive({ # Exculsive対応
    # Plot the kept and excluded points as two separate data sets
    keep    <- score.train[ vals$keeprows, , drop = FALSE]
    exclude <- score.train[!vals$keeprows, , drop = FALSE]
    
    x.label <- xvar()
    y.label <- yvar()
    
    
    x.keep <- keep[, x.label]
    y.keep <- keep[, y.label]
    x.exclude <- exclude[, x.label]
    y.exclude <- exclude[, y.label]
    
    # ----- ログ取得用  -----
    save_logData(getUserName(), "current labels", x.label, y.label)
    # -----------------------
    
    
    ggplot(exclude, aes(x.exclude, y.exclude, color = rank)) +
      geom_point(shape = 1, alpha = 0.5) +
      labs( x = x.label, y = y.label ) +
      #geom_point(exclude, hape = 21, fill = NA, color = "white", alpha = 0.25) +
      layer(data =  keep,
            mapping = aes(x.keep,  y.keep),
            geom = "point",
            position = "identity",
            stat = "identity",
      )
  })
  
  
  # ----- reactive関数：TestPlot描画  -----
  drawTestPlot2 <- reactive({ # Exculsive対応
    # Plot the kept and excluded points as two separate data sets
    keep    <- score.train[ vals$keeprows, , drop = FALSE]
    exclude <- score.train[!vals$keeprows, , drop = FALSE]
    
    x.label <- xvar()
    y.label <- yvar()
    
    # x.keep <- keep[, x.label]
    # x.exclude <- exclude[, x.label]
    # x.test <- score.test[, x.label]
    # x.toPlot <- rbind(x.keep, x.exclude, x.test)
    # 
    # y.keep <- keep[, y.label]
    # y.exclude <- exclude[, y.label]
    # y.test <- score.test[, y.label]
    
    data.toPlot <- rbind(keep, score.test, exclude)
    data.type <- c(rep("keep", NROW(keep)), rep("score.test", NROW(score.test)), rep("exclude", NROW(exclude)))
    data.toPlot <- cbind(data.toPlot, data.type)
    
    x.toPlot <- data.toPlot[, x.label]
    y.toPlot <- data.toPlot[, y.label]
    
    #data.type <- c(rep("keep", nrow(x.keep)), rep("exclude", nrow(x.exclude)), rep("test", nrow(x.test)))
    # data.type <- c(rep("keep",NROW(x.keep)), rep("exclude", NROW(x.exclude)), rep("test", NROW(x.test)))
    # x.toPlot <- cbind(x.toPlot, data.type)
    # ----- ログ取得用  -----
    save_logData(getUserName(), "test plot", x.label, y.label)
    # -----------------------
    
    cols <- c("keep" = 16, "test" = 8, "exclude" = 1)
    ggplot(data.toPlot, aes(x.toPlot, y.toPlot, color = rank, shape = data.type)) +
      geom_point() +
      labs( x = x.label, y = y.label ) +
      scale_shape_manual(values=cols)
    # ggplot(keep, aes(x.keep, y.keep, color = rank)) + 
    #   geom_point(shape = 1, alpha = 0.5) +
    #   labs( x = x.label, y = y.label ) +
    #   #geom_point(exclude, hape = 21, fill = NA, color = "white", alpha = 0.25) +
    #   layer(data =  score.test,
    #         mapping = aes(x.test,  y.test),
    #         geom = "point",
    #         position = "identity",
    #         stat = "identity",
    #   ) 
  })
  
  
  
  # ------  散布図データをクリック  -----
  # Toggle points that are clicked and show infomation
  observeEvent(input$plot_click, {
    if (input$clickMode == "delete") {
      res <- nearPoints(score.train, input$plot_click, xvar(), yvar(), allRows = TRUE, maxpoints = 1)
      # ----- ログ取得用  -----
      save_logData(getUserName(),"delete a record",  res[res$selected_ == TRUE, 2], "")
      # -----------------------
      vals$keeprows <- xor(vals$keeprows, res$selected_)
    }
    
    output$click_info <- renderPrint({
      # Select just the nearest point within 10 pixels of the click
      res <- nearPoints(score.train, input$plot_click, xvar(), yvar(), threshold = 10, maxpoints = 1)
      # ----- ログ取得用  -----
      save_logData(getUserName(),"reference a record", res[1], "")
      # -----------------------
      res[1:4]
    })
  })
  
  
  # -----  線形判別の計算  -----
  # 返り値：線形判別後の分類精度accuracy
  ldaExe <- reactive( {
    keep    <- score.train[ vals$keeprows, , drop = FALSE]
    exclude <- score.train[!vals$keeprows, , drop = FALSE]
    
    x.keep <- keep[, xvar()]
    y.keep <- keep[, yvar()]
    x.exclude <- exclude[, xvar()]
    y.exclude <- exclude[, yvar()]
    
    
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
    
    tbl <- table(score.test$V1, pre)
    # lda: 同じ変数を指定するとWarning..variables are collinear発生
    
    #sum <- sum(rowSums(tbl))
    #correct <- tbl[1,1] + tbl[2,2]
    #accuracy <- (correct / sum) * 100
  })
  
  # ----- 散布図描画  ------
  output$plot <- renderPlot({
    p <- drawPlot2()
    ldaExe() # pを表示する前に入れないとggplotが消える
    p
  })
  
  # return Name of the x, y
  xvar <- reactive({
    switch(input$xlabel, math = "math", english = "english", japanese = "japanese")
  })
  yvar <- reactive({
    switch(input$ylabel, math = "math", english = "english", japanese = "japanese")
  })
  
  # ----- 排除された行を記録  -----
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(score.train))
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
  
  # ----- 精度出力  -----
  output$accuracy <- renderTable({ 
    acc <- showAccuracy()
    # ----- ログ取得用  -----
    save_logData(getUserName(),"acc[1,1], acc[1,2]",  acc[1,3], acc[2,3])
    save_logData(getUserName(),"acc[2,1], acc[2,2]",  acc[3,3], acc[4,3])
    # -----------------------
    acc11 <- c("upper", "upper", acc[1,3])
    acc12 <- c("lower", "upper", acc[2,3])
    acc13 <- c("upper", "lower", acc[3,3])
    acc14 <- c("lower", "lower", acc[4,3])
    acc <- rbind(acc11, acc12, acc13, acc14)
    acc <- as.data.frame(acc)
    colnames(acc) <- c("正解ラベル","予測ラベル", "データ数")
    acc
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
  output$accuracy2 <- renderTable({ 
    acc <- getAccuracy()
    # ----- ログ取得用  -----
    save_logData(getUserName(),"Test: acc[1,1], acc[1,2]",  acc[1,3], acc[2,3])
    save_logData(getUserName(),"Test: acc[2,1], acc[2,2]",  acc[3,3], acc[4,3])
    # -----------------------
    acc11 <- c("upper", "upper", acc[1,3])
    acc12 <- c("lower", "upper", acc[2,3])
    acc13 <- c("upper", "lower", acc[3,3])
    acc14 <- c("lower", "lower", acc[4,3])
    acc <- rbind(acc11, acc12, acc13, acc14)
    acc <- as.data.frame(acc)
    colnames(acc) <- c("正解ラベル","予測ラベル", "データ数")
    acc
  })

  # -----  テストデータの描画  -----
  output$test_plot <- renderPlot({
    drawTestPlot2()
  })
  
  # -----  テストデータ用散布図情報  -----
  output$click_info2 <- renderPrint({
    res <- nearPoints(score, input$plot_click2, xvar(), yvar(), threshold = 10, maxpoints = 1)
    # ----- ログ取得用  -----
    save_logData(getUserName(),"reference a record (testPlot)", res[1], "")
    # -----------------------
    res[1:4]
  })
  
})
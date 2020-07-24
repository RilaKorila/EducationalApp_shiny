shinyServer(function(input, output, session) {
  
  # set help content
  session$sendCustomMessage(type = 'setHelpContent', message = list(steps = toJSON(steps) ))
  
  # listen to the action button
  observeEvent(input$startHelp,{
    # on click, send custom message to start help
    session$sendCustomMessage(type = 'startHelp', message = list(""))
  })
  
  score <- read.csv("/path/to/data.csv", header = TRUE)
  # ------  ログ取得用  -------
  #username <- "user1"
  filename <- "test.csv"
  name.col.list <- c("name", "date", "time", "action", "value1", "value2")
  df <- data.frame(matrix(rep(NA, length(name.col.list)), nrow=1))[numeric(0), ]
  colnames(df) <- name.col.list
  x <- as.data.frame(df)
  write.table(x, file = filename, sep = ",")
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
    username <- input$username
    # ----- ログ取得用  -----
    date <- format(Sys.time(), "%Y/%m/%d")
    time <- format(Sys.time(), "%H:%M:%OS")
    x <- matrix(c(username, date, time, "register a username", username, ""),
                nrow = 1, ncol = 6)
    write.table(x, file = filename, append = TRUE, sep = ',',
                row.names = FALSE, col.names = FALSE)
    # -----------------------
    
    # 操作盤のロック解除
    enable("xlabel")
    enable("ylabel")
    enable("clickMode")
    # 名前の変更をロック
    disable("username")
    disable("registerBtn")
  })
  
  drawPlot2 <- reactive({ # Exculsive対応
    # Plot the kept and excluded points as two separate data sets
    keep    <- score[ vals$keeprows, , drop = FALSE]
    exclude <- score[!vals$keeprows, , drop = FALSE]
    
    x.label <- xvar()
    y.label <- yvar()
    
    x.keep <- keep[, x.label]
    y.keep <- keep[, y.label]
    x.exclude <- exclude[, x.label]
    y.exclude <- exclude[, y.label]
    
    # ----- ログ取得用  -----
    username <- getUserName()
    date <- format(Sys.time(), "%Y/%m/%d")
    time <- format(Sys.time(), "%H:%M:%OS")
    x <- matrix(c(username, date, time, "current labels", x.label, y.label),
                nrow = 1, ncol = 6)
    write.table(x, file = filename, append = TRUE, sep = ',',
                row.names = FALSE, col.names = FALSE)
    # -----------------------
    
    ggplot(exclude, aes(x.exclude, y.exclude, color = rank)) + 
      geom_point(shape = 1, alpha = 0.5) +
      #geom_point(exclude, hape = 21, fill = NA, color = "white", alpha = 0.25) +
      layer(data =  keep,
            mapping = aes(x.keep,  y.keep),
            geom = "point",
            position = "identity",
            stat = "identity",
      )  
    
    
  })
  
  
  # Toggle points that are clicked and show infomation
  observeEvent(input$plot_click, {
    if (input$clickMode == "delete") {
      res <- nearPoints(score, input$plot_click, xvar(), yvar(), allRows = TRUE, maxpoints = 1)
      # ----- ログ取得用  -----
      username <- getUserName()
      date <- format(Sys.time(), "%Y/%m/%d")
      time <- format(Sys.time(), "%H:%M:%OS")
      x <- matrix(c(username, date, time, "delete a record", res[res$selected_ == TRUE, 2], ""),
                  nrow = 1, ncol = 6)
      write.table(x, file = filename, append = TRUE, sep = ',',
                  row.names = FALSE, col.names = FALSE)
      # -----------------------
      vals$keeprows <- xor(vals$keeprows, res$selected_)
    }
    
    output$click_info <- renderPrint({
      # Select just the nearest point within 10 pixels of the click
      res <- nearPoints(score, input$plot_click, xvar(), yvar(), threshold = 10, maxpoints = 1)
      # ----- ログ取得用  -----
      # if (length(res) != 0){
      username <- getUserName()
      date <- format(Sys.time(), "%Y/%m/%d")
      time <- format(Sys.time(), "%H:%M:%OS")
      x <- matrix(c(username, date, time, "reference a record", res[1], ""),
                  nrow = 1, ncol = 6)
      write.table(x, file = filename, append = TRUE, sep = ',',
                  row.names = FALSE, col.names = FALSE)
      # }
      # -----------------------
      res[1:4]
    })
  })
  
  # 線形判別の計算
  # 返り値：線形判別後の分類精度accuracy
  # 返り値2: table
  ldaExe <- reactive( {
    keep    <- score[ vals$keeprows, , drop = FALSE]
    exclude <- score[!vals$keeprows, , drop = FALSE]
    
    x.keep <- keep[, xvar()]
    y.keep <- keep[, yvar()]
    x.exclude <- exclude[, xvar()]
    y.exclude <- exclude[, yvar()]
    
    # actionButtonに反応するためのダミー変数
    tmp <- input$calAccuracy
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
    
    tbl <- table(score.test$V1, pre)
    # lda: 同じ変数を指定するとWarning..variables are collinear発生
    
    #sum <- sum(rowSums(tbl))
    #correct <- tbl[1,1] + tbl[2,2]
    #accuracy <- (correct / sum) * 100
  })
  
  # ActionButtonが押されて初めてboundaryのデータが更新される
  boundaryData <- eventReactive(input$boundary, {
    a <- input$intercept
    b <- input$slope
    a.b <- c(a, b)
    a.b
  })
  
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
  
  # For storing which rows have been excluded(排除された行を記録)
  vals <- reactiveValues(
    #もしかしてExcludeできるのはscore.trainの方だけ？
    keeprows = rep(TRUE, nrow(score))
  )
  
  
  output$accuracy <- renderTable({ 
    tbl <-  ldaExe()
    tbl <- as.data.frame(tbl)
    colnames(tbl) <- c("正解ラベル","予測ラベル", "データ数")
    tbl
  })
  
  
  
})
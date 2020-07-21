#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
# Define UI for application
shinyUI(fluidPage(
    # shinyjsを使うことを宣言
    useShinyjs(),
    # Introjs用
    # tags$head(includeHTML("googleAnalytics.html"))
    
    # Application title
    titlePanel("title panel"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        # Show a plot of the generated distribution
        mainPanel(
            #clickId, hoverIdはggplotでは使用不可
            plotOutput("plot", click = "plot_click"),
            # iPadで使うならhoverできない
            hr(),                               
            column(width = 10,
                   h4("Points near click"),
                   verbatimTextOutput("click_info")
            )
        ),
        
        # Show slidebar
        sidebarPanel( # 色の種類はcolors()でみれる
            h3("ユーザー名登録", style = "color:skyblue"),
            textInput("username", "ユーザー名", placeholder = "Please input your name"),
            actionButton("registerBtn", "登録" ),
            h3("変数選択", style = "color:skyblue"),
            # 変数選択x軸
            disabled(
            selectInput("xlabel", label = "x軸", 
                         c("Math" = "math",
                           "English" = "english",
                           "Japanese" = "japanese"))
            ),
            # 変数選択y軸
            disabled(
            selectInput("ylabel", label = "y軸", 
                        c("Math" = "math",
                          "English" = "english",
                          "Japanese" = "japanese"))
            ),
            
            # -----  データの削除 -----
            hr(),    
            disabled(
            radioButtons("clickMode", 
                         label = h3("データの削除",  style = "color:skyblue"),
                         c( "データ参照" = "normal",
                            "データ変更" = "delete"))
            ),
           
            # ----- 精度の表示 -----
            hr(),  
            h3("分類精度", style = "color:skyblue"),
            actionButton("calAccuracy", "精度算出"),
            tableOutput("accuracy"),
            
            # ------ 境界線描画 -----
            hr(),    
            h3("境界線描画", style = "color:skyblue"),
            # 数値入力
            numericInput("slope", label = "傾き", value = 1, 
                         min = 1, max = 100, step = 1),
            # 切片入力
            numericInput("intercept", label = "切片", value = 1, 
                         min = 1, max = 100, step = 1),
            actionButton("boundary", "Let's draw!"),
            actionButton("HideBoundary", "Hide"),
            # ------------------------
            
           
            )
        
        
    )
))

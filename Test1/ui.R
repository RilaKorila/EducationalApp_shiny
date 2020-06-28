#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
shinyUI(fluidPage(
    
    # Application title
    titlePanel("title panel"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        # Show a plot of the generated distribution
        mainPanel(
            #clickId, hoverIdはggplotでは使用不可
            plotOutput("plot", click = "plot_click"),
            # iPadで使うならhoverできない
            
            
            #textOutput("xlabel_get"),
            #textOutput("ylabel_get"),
            
            #textInput("textIn01",label="Input"),
            hr(),                               
            column(width = 10,
                   h4("Points near click"),
                   verbatimTextOutput("click_info")
            )
        ),
        
        # Show slidebar
        sidebarPanel(
            h3("変数選択", style = "color:skyblue"),
            # 変数選択x軸
            selectInput("xlabel", label = "x軸", 
                         c("Math" = "math",
                           "English" = "english",
                           "Japanese" = "japanese")),
            # 変数選択y軸
            selectInput("ylabel", label = "y軸", 
                        c("Math" = "math",
                          "English" = "english",
                          "Japanese" = "japanese")),
            # 色の種類はcolors()でみれる
            hr(),    
            # 精度の表示
            h4("精度"),
            tableOutput("accuracy"),
            # tags$table(
            #     tags$tr(
            #         tags$th("pre"), tags$th("real")
            #     ),
            #     tags$tr(
            #         textOutput("tbl11"), tags$th("tbl12")
            #     ),
            #     tags$tr(
            #         tags$th("tbl21"), tags$th("tbl22")
            #     ),
            # ),
            
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
            hr(),    
            # ボタン
            h3("データの削除", style = "color:skyblue"),
            checkboxInput("deleteMode", label = "Delete", FALSE),
            )
        
    )
))

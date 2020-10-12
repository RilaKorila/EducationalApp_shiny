shinyUI(fluidPage(
  # theme = shinythemes::shinytheme("flatly"),
  #theme = "bootstrap.min.css",
  h4("訓練データ", style = "color:steelblue; text-align: center;"),
  column(2),
  column(8,
         plotOutput("plot", 
                    click = "plot_click",
                    brush = brushOpts(id = "plot_brush",
                                      opacity = 0.4,)),
         ),
  column(2),
  
                                                             
  # column(width = 12,
  #        verbatimTextOutput("brush_info")
  # ),
  # column(width = 12,
  #        verbatimTextOutput("click_info")
  # ),
  hr(),
  column(9,
         h4("境界線を引くと...", style = "color:steelblue; text-align: center;"),
         plotOutput("plot_output")
  ),
  column(3, 
         h4("精度を確認する", style = "color:steelblue; text-align: center;"),
         #h4("テストデータを表示", style = "color:steelblue; text-align: center;"),
         checkboxInput("plotTest", "テストデータを表示する", FALSE),
         hr(),
         div(style = "text-align: center;",
             actionButton("calAccuracy", "精度を計算")),
         div(style = "text-align: center; font-size: 2em; padding: 5px;",
             textOutput("accuracy")
         ),
         tableOutput("accuracyTable")
         )
))

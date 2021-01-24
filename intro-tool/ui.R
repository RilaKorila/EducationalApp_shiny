shinyUI(fluidPage(
  # shinyjsを使うことを宣言
  useShinyjs(),

  h4("訓練データ", style = "color:steelblue; text-align: center;"),
  column(2),
  column(8,
         plotOutput("plot", 
                    click = "plot_click",
                    brush = brushOpts(id = "plot_brush",
                                      opacity = 0.4,)),
         ),
  column(2),
  
  hr(),
  column(9,
         h4("境界線を引くと...", style = "color:steelblue; text-align: center;"),
         plotOutput("plot_output")
  ),
  column(3, 
         h4("精度を確認する", style = "color:steelblue; text-align: center;"),
         disabled(checkboxInput("plotTest", "テストデータを表示", FALSE)),
         )
  
))

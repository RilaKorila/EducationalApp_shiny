shinyUI(
    navbarPage(title = "Music Analysis",
               theme = shinythemes::shinytheme("flatly"),
              
               id = "step8",
               # --------- username登録タブ  ---------
               tabPanel(title = "User Name",
                        fluidPage(
                          # Include IntroJS styling
                          includeCSS("introjs.css"),
                          useShinyjs(),
                          
                          fluidRow(
                            h2("ユーザー名登録", style = "color:steelblue; text-align: left; margin-top: 10px; margin-bottom: 20px;"),
                            tags$p("最初に、ユーザー名を登録お願いします。", style = "margin-bottom: 0px;"),
                            textInput("username", NULL, placeholder = "Your name")),
                            textOutput("message")
                        
                          )
                        ),
               # --------- ここからタブ1  ---------
               tabPanel(title = "Plot",
                        fluidPage(
                            # Include styling for the app
                            includeCSS("app.css"),
                            
                            # Include IntroJS library
                            includeScript("intro.js"),
                            
                            # Include javascript code to make shiny communicate with introJS
                            includeScript("app.js"),
                            
                            fluidRow(
                                # mainPanel
                                column(8, div(class = "mainPanel", style = "margin: 0;",
                                              fluidRow(
                                                  div(id="step3", class="well", 
                                                      plotOutput("plot", click = "plot_click", hover = hoverOpts(id = "plot_hover",
                                                                                                                 delay = 200))
                                                  ),
                                                  hr(),
                                                  h4("データの詳細情報", style = "color:steelblue;"),
                                                  div(id="step4", class="well", 
                                                      tableOutput("click_info")
                                                  )
                                              ))
                                ), 
                                # sub Panel
                                column(4, div(class = "subPanel",
                                              fluidRow(
                                                # centered botton
                                                div(style = "text-align: center;",
                                                  actionButton(inputId="startHelp", label="操作方法確認",  width = 200)),
                                                                            
                                                h4("変数選択", style = "color:steelblue; text-align: center;"),
                                                  
                                                div(id="step6", class="well", style = "margin: 10px; padding:10px;",
                                                    disabled(selectInput("xlabel", label = "x軸", 
                                                                         c("umap_x" = "umap_x", "umap_y" = "umap_y"))),
                                                    disabled(selectInput("ylabel", label = "y軸", 
                                                                         c("umap_y" = "umap_y", "umap_x" = "umap_x")))
                                                ),
                                                h4("分類精度", style = "color:steelblue; text-align: center;"),
                                                div(id="step7", class="well", style = "margin: 10px; padding:10px;",
                                                    tableOutput("accuracy")
                                                ),
                                                h4("削除データ数", style = "color:steelblue; text-align: center;"),
                                                div(id="step7", class="well", style = "margin: 10px; padding:10px;",
                                                    textOutput("deleteNum")
                                                ),
                                              )
                                ))
                            ))),# -----  ここまで Plot タブ  -----
                            
                            # -----  ここから Test タブ  -----
                            tabPanel("Test",
                                     fluidRow(
                                         # mainPanel
                                         column(8, div(class = "mainPanel", 
                                                       fluidRow(
                                                         div(class = "well",
                                                           plotOutput("test_plot", hover = hoverOpts(id = "plot_hover2",delay = 200))
                                                        ),
                                                        hr(),
                                                         h4("データの詳細情報", style = "color:steelblue;"),
                                                         div(class = "well",
                                                               tableOutput("click_info2")
                                                             )
                                                           ))
                                         ), 
                                         
                                         # sub Panel
                                         column(4, div(class = "subPanel",
                                                       fluidRow(
                                                           h4("分類精度", style = "color:steelblue; text-align: center;"),

                                                           div(class = "well",
                                                               tableOutput("accuracy2")
                                                               ),
                                                           hr(),
                                                           tags$div(class = "well",
                                                                    "境界線と＊印のテストデータの色分けに注目してみてください。",
                                                                    tags$br(),
                                                                   "境界線が適切に移動すれば、正しく分類されるデータが増えるかもしれません。"
                                                                
                                                       )
                                         )
                                         ))
                            )),
               # -----  ここから Deleted Data タブ  -----
               tabPanel("Deleted Data",
                 fluidRow(
                   column(12,
                          tableOutput("deletedData")
                          )
                 )
               )
)
)
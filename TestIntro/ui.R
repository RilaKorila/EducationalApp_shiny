shinyUI(fluidPage(
    # shinyjsを使うことを宣言
    useShinyjs(),
    
    # Include IntroJS styling
    includeCSS("introjs.css"),
    
    # Include styling for the app
    includeCSS("app.css"),
    
    # Include IntroJS library
    includeScript("intro.js"),
    
    # Include javascript code to make shiny communicate with introJS
    includeScript("app.js"),
    
    # set Title panel
    titlePanel("Title Panel"),

    # centered botton
    div(class="flexcontainer", 
        # action button
        actionButton(inputId="startHelp", label="start", class="btn-success")
    ),
    
    fluidRow(
        # mainPanel
        column(8, div(class = "mainPanel", 
            fluidRow(
                div(id="step1", class="well", 
                    plotOutput("plot", click = "plot_click"),
                    ),
                div(id="step2", class="well", 
                    h4("Points near click"),
                    verbatimTextOutput("click_info")
                    ),
                div(id="step3", class="well", "element3")
            ))
        ), 
        
        # sub Panel
        column(4, div(class = "subPanel",
            fluidRow(
                h3("ユーザー名登録", style = "color:skyblue"),
                div(id="step4", class="well", 
                    textInput("username", "ユーザー名", placeholder = "Please input your name"),
                    actionButton("registerBtn", "登録" )
                    ),
                h3("変数選択", style = "color:skyblue"),
                
                div(id="step5", class="well", 
                    disabled(
                    selectInput("xlabel", label = "x軸", 
                                c("Math" = "math",
                                  "English" = "english",
                                  "Japanese" = "japanese"))),
                    disabled(
                    selectInput("ylabel", label = "y軸", 
                               c("Math" = "math",
                                 "English" = "english",
                                 "Japanese" = "japanese")))
                    ),
                div(id="step6", class="well", 
                    disabled(
                    radioButtons("clickMode", 
                                 label = h3("データの削除",  style = "color:skyblue"),
                                 c( "データ参照" = "normal",
                                    "データ変更" = "delete")))
                    ),
                div(id="step7", class="well", 
                    disabled(
                    actionButton("calAccuracy", "精度算出")),
                    tableOutput("accuracy")
                )
            )
        ))
    ),
    # # row 3
    # fluidRow(
    #     column(4, div(id="step7", class="well", "element7")),
    #     column(4, div(id="step8", class="well", "element8")),
    #     column(4, div(id="step9", class="well", "element9"))
    # ),
    
)
)
# ui.R
shinyUI(
  dashboardPagePlus(skin="red",
                    dashboardHeader(title = "幸福的青鳥(v1.0)"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("範例",tabName = "description",icon = icon("dashboard")),
                        menuItem("迴歸模型",tabName="model",icon = icon("calculator")),
                        menuItem("資料視覺化",tabName="eda",icon=icon("area-chart")),
                        menuItem("機器學習",tabName="machine",icon=icon("abacus")),
                        menuItem("自動爬蟲",tabName="mining",icon = icon("sitemap")),
                        menuItem("歐式距離",tabName="dist",icon=icon("stethoscope"))
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$style(
                          HTML("h1{font-size:'Microsoft Yahei';color:blue;}")
                        )
                      ),
                      tabItems(
                        tabItem(tabName = "description",
                                fluidRow(
                                  tabBox(title = "",width = 12,
                                         tabPanel(title="地圖範例",width=8,
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      helpText("請選擇查詢目標"),
                                                      selectInput("x","請選擇查詢都市",c("NULL","台北",'高雄'),multiple=FALSE),
                                                      selectInput("xx","請選擇查詢都市",c("NULL","中正區",'文山區'),multiple=FALSE),
                                                      checkboxInput('inputId','只顯示出租中的房子', value = FALSE, width = NULL)
                                                    ),
                                                    mainPanel(
                                                      leafletOutput("mymap"))
                                                  )
                                         ))
                                )),
                        tabItem(tabName = "model",
                                h1("Regression Model"),
                                fluidRow(
                                  box(width = 6,height = "200px",title="Upload File",solidHeader = T,status = "primary",
                                      fileInput('file1', 'Choose CSV file',
                                                accept=c('text/csv', 
                                                         'text/comma-separated-values,text/plain', 
                                                         '.csv')),
                                      uiOutput('header'),
                                      helpText("Upload data saved by encoding:BIG-5,and to store in data base,0.5MB limit.")
                                  ),
                                  box(width = 6,height = "200px",title="Instruction",solidHeader = T,status = "info",
                                      uiOutput("dependent"),
                                      uiOutput("independents"),
                                      p("to be continue...")
                                  ),
                                  tabBox(title = "",width = 12,
                                         tabPanel(title="資料型態",
                                                  verbatimTextOutput('str'),
                                                  tableOutput('data')
                                         ),
                                         tabPanel(title="敘述統計",
                                                  verbatimTextOutput('basic')
                                         ),
                                         tabPanel(title = "迴歸模型",
                                                  verbatimTextOutput('contents')
                                         ),
                                         tabPanel(title = "殘差檢定",
                                                  plotOutput('residual')
                                         ),
                                         tabPanel(title="變異數分析",
                                                  tableOutput('anova')
                                         ),
                                         tabPanel(title="模型篩選",
                                                  tableOutput('select')
                                                  
                                         ),
                                         tabPanel(title = '相關係數矩陣',
                                                  plotOutput('corrplot')
                                         ))
                                )),
                        tabItem(tabName = "eda",
                                h1("EDA"),
                                fluidRow(
                                  box(width = 6,height = "200px",title = "Upload File",solidHeader = T,status = "primary",
                                      fileInput('file2', 'Choose CSV file',
                                                accept=c('text/csv', 
                                                         'text/comma-separated-values,text/plain', 
                                                         '.csv')),
                                      helpText("Upload data saved by encoding:utf-8,and to store in data base,0.5MB limit.")
                                  ),
                                  box(width = 6,height = "200px",title="Instruction",solidHeader = T,status = "info"
                                  ),
                                  tabBox(title = "",width = 12,
                                         tabPanel(title = "折線圖",
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      uiOutput("y"),
                                                      uiOutput("x"),
                                                      uiOutput("factor"),
                                                      tags$b(h5("Notice:")),
                                                      helpText("if choose continuous variable as 'factor', it will print error")
                                                      
                                                    ),
                                                    mainPanel(
                                                      plotOutput("multiplot"))
                                                  )
                                         ),
                                         tabPanel(title="盒狀圖",
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      uiOutput("y.box"),
                                                      uiOutput("x.box"),
                                                      uiOutput("factor.box"),
                                                      tags$b(h5("Notice:"))
                                                    ),
                                                    mainPanel(
                                                      plotOutput('boxplot')
                                                    )
                                                  )
                                                  
                                         ),
                                         tabPanel(title="直方圖",
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      numericInput('bins',"bins",10,min=1,max=30),
                                                      uiOutput("x.hist"),
                                                      uiOutput("factor.hist")
                                                    ),
                                                    mainPanel(
                                                      plotOutput('hist')
                                                    )
                                                  ))
                                  )
                                )),
                        tabItem(tabName = "machine",
                                h1("Machine Learning"),
                                fluidRow(
                                  box(width = 6,height = "200px",title = "Upload File",solidHeader = T,status = "primary",
                                      fileInput('file_machine', 'Choose CSV file',
                                                accept=c('text/csv', 
                                                         'text/comma-separated-values,text/plain', 
                                                         '.csv')),
                                      helpText("Upload data saved by encoding:BIG-5,and to store in data base,0.5MB limit.")
                                  ),
                                  box(width = 6,heiht="200px",title="Instruction",solidHeader = T,status="success",
                                      p("該來打啥勒~~")
                                  )
                                  ,
                                  tabBox(width = 12,title="",
                                         tabPanel(title="決策樹",
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      uiOutput("y.rpart"),
                                                      uiOutput("x.rpart"),
                                                      tags$b(h5("Notice:")),
                                                      actionButton("go","GO!")
                                                    ),
                                                    mainPanel(
                                                      plotOutput('rpartplot')
                                                    )
                                                  )
                                         ),
                                         tabPanel(title="隨機森林",
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      uiOutput("y.forest"),
                                                      uiOutput("x.forest"),
                                                      tags$b(h5("Notice:")),
                                                      actionButton("go.forest","GO!")
                                                    ),
                                                    mainPanel(
                                                      plotOutput("importanceplot"),
                                                      plotOutput("forestplot")
                                                    )
                                                  )
                                         )
                                  ))
                        ),
                        tabItem(tabName = "mining",
                                h1("DataMining"),
                                fluidRow(
                                  loadingState(),
                                  box(width = 6,height = "200px",title = "Crawler Board",solidHeader = TRUE,status = "primary",
                                      selectInput("board","Choose Board",
                                                  choices = c("NULL","Gossiping","HatePolitics","NBA","Stock","movie","C_Chat","LoL","WomenTalk","Baseball",
                                                              "PC_Shopping","joke","Beauty")
                                      ),
                                      actionButton("action","GO!")
                                  ),
                                  box(width=6,height = "200px",title = "Crawler Page",solidHeader = T,status = "info",
                                      numericInput('page',"choose page",5,min=0,max=50),
                                      downloadButton("download","Download")),
                                  h2("Result"),
                                  tabBox(title = "",width = 12,
                                         tabPanel(title = "Data",
                                                  p("Nums of Article"),
                                                  verbatimTextOutput('nums'),
                                                  tableOutput('test')))
                                  
                                )
                        ))
                    )))

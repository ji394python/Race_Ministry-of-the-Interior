# ui.R
shinyUI(
  dashboardPagePlus(skin="red",
                    dashboardHeader(title = "幸福的青鳥(v1.0)"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("範例",tabName = "description",icon = icon("dashboard"))
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$style(
                          HTML("h1{font-size:'Microsoft Yahei';color:blacke;}")
                        )
                      ),
                      tabItems(
                        tabItem(tabName = "description",
                                fluidRow(
                                  tags$p("目前只開放精華里/中正里兩者的各身分別查詢"),
                                  box(width = 4,height = "200px",title = "讓我們更了解您",solidHeader = T,status = "primary",
                                      fluidRow(
                                        column(width=6,
                                        selectInput("person","您的身分",c("NULL","單身貴族",'新婚夫妻',"學生","高齡人士"),multiple=FALSE)),
                                        column(width=6,
                                        uiOutput("second"))
                                        ),
                                      fluidRow(
                                        column(width=2,
                                        actionButton("go.forest","search!")))
                                  ),
                                  box(width = 8,height = "200px",title = "請選擇查詢地址",solidHeader = T,status = "primary",
                                      fluidRow(
                                        column(width = 6,
                                        selectInput("city","查詢城市",c("NULL","台北","新北","桃園","台中","台南","高雄"),multiple=FALSE)),
                                        column(width = 6,
                                        uiOutput("city_second")),
                                        column(width = 6,
                                        uiOutput("city_third")),
                                        column(width =6,
                                        uiOutput("city_four"))
                                      )
                                  ),
                                  tabBox(title = "",width = 12,
                                         tabPanel(title="地圖範例",width=12,
                                                  fluidRow(
                                                    column(width = 9,
                                                           leafletOutput("mymap")),
                                                    column(width=3,
                                                           fluidRow(
                                                            h4(textOutput(outputId = "text")),
                                                           uiOutput(outputId = "my_ui_title"),
                                                           br(),
                                                           uiOutput(outputId = "my_ui_score")
                                                           )
                                                         )
                                                  ))
                                                  
                                         ))
                                ))
                    )))

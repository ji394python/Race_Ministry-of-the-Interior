shinyServer(function(input, output) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles() %>% addMarkers(map, lng=120.239, lat=22.992, popup="123")
  })
  #---------------first page----------------#
  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)){return(print("The input files are null,please try again"))}
    read.csv(infile$datapath,fileEncoding = 'BIG5')
  })
  output$header <- renderUI({
    df <- filedata() 
    if (is.null(df)) {return(checkboxInput('header','Header(first row)',TRUE))}
  })
  output$dependent <- renderUI({
    df <- filedata()
    if (is.null(filedata())) return(NULL)
    items=names(filedata())
    names(items)=items
    selectInput("dependent","Choose one dependent(Y)",items)
  })
  output$independents <- renderUI({
    df <- filedata()
    if (is.null(df)) return()
    items=names(df)
    names(items)=items
    selectInput("independents","Choose one or multiple independents(X)",items,multiple=TRUE)
  })
  output$str <- renderPrint({
    df <- filedata()
    if (is.null(df)) return()
    str(df)
  })
  output$data <- renderTable({
    if(is.null(filedata)) return()
    head(filedata())
  })
  output$basic <- renderPrint({
    if(is.null(filedata)) return("NULL")
    summary(filedata())
  })
  output$contents <- renderPrint({
    df <- filedata()
    if (is.null(df)) return()
    fmla <- as.formula(paste(input$dependent," ~ ",paste(input$independents,collapse="+")))
    summary(lm(fmla,data=df))
  })
  output$residual <- renderPlot({
    df <- filedata()
    if (is.null(df)) return()
    fmla <- as.formula(paste(input$dependent," ~ ",paste(input$independents,collapse="+")))
    par(mfrow=c(2,2))
    plot(lm(fmla,data=df))
  })
  output$corrplot <- renderPlot({
    df <- filedata()
    if (is.null(df)) return()
    df %>% select(c(input$dependent,input$independents)) -> cd
    pairs.panels(cd)
  })
  
  anova <- reactive({
    df <- filedata()
    vif <- vector()
    if (is.null(df)) return()
    fmla <- as.formula(paste(input$dependent," ~ ",paste(input$independents,collapse="+")))
    fmla <- lm(fmla,data=df)
    a <- vector()
    for (i in 1:fmla$rank) {a[i]=1}
    vif <- c(0,vif(fmla))
    a <- data.frame(variable=names(coefficients(fmla)),df=a,coefficients=coefficients(fmla),summary(fmla)$coefficients[,2:4],
                    confint(fmla, level=0.95),vif)
    
  })
  output$anova <- renderTable({
    anova()
  })
  
  output$select <- renderTable({
    df <- filedata()
    if (is.null(df)) return()
    print <- summary(regsubsets(as.formula(paste(input$dependent," ~ ",paste(input$independents,collapse="+"))),data=df,nbest = 2))
    table <- print$which
    for (i in 1:ncol(table)){
      table[,i] <- as.numeric(table[,i])
    }
    data.frame(x1=table,Rsq=print$rsq,Rss=print$rss,AdjR=print$adjr2,
               Cp=print$cp,Bic=print$bic)
  })
  
  #------------------End--------------------#
  
  #---------------second page----------------#
  file1data <- reactive({
    infile <- input$file2
    if (is.null(infile)){return()}
    read.csv(infile$datapath,fileEncoding = 'BIG5')
  })
  output$y <- renderUI({
    df <- file1data()
    if (is.null(file1data())) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("y","Choose one dependent(Y)",c("NULL",items))
  })
  output$x <- renderUI({
    df <- file1data()
    if (is.null(df)) return()
    items=names(df)
    names(items)=items
    selectInput("x","Choose one or multiple independents(X)",c("NULL",items),multiple=TRUE)
  })
  output$factor <- renderUI({
    df <- file1data() 
    if(is.null(df)) {return()}
    items=names(df)
    names(items)=items
    selectInput("factor","Choose one factor variable",c("NULL",items))
  })
  output$multiplot <- renderPlot({
    df <- file1data()
    if (is.null(df)) return()
    storenames <- list()
    storeplot  <- list()
    for (i in 1:length(input$x)){
      storenames[i] = input$x[i]
    }
    for (i in 1:length(storenames)){
      storeplot[[i]] <- ggplot(data=df,aes_string(x=as.character(storenames[i]),
                                                  y=as.character(input$y),col=input$factor,shape=input$factor))+
        geom_point(shape=20) + geom_smooth(method = lm)
      storeplot[[i]] <- storeplot[[i]]
    }
    
    do.call(grid.arrange,c(storeplot,ncol=2))
    
  })
  
  output$x.box <- renderUI({
    df <- file1data()
    if (is.null(df)) return()
    items=names(df)
    names(items)=items
    selectInput("x.box","Choose one or more independent(x)",c("NULL",items),multiple = TRUE)
  })
  output$y.box <- renderUI({
    df <- file1data()
    if (is.null(file1data())) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("y.box","Choose one dependent(Y)",c("NULL",items))
  })
  output$factor.box <- renderUI({
    df <- file1data() 
    if(is.null(df)) {return()}
    items=names(df)
    names(items)=items
    selectInput("factor.box","Choose one factor variable",c("NULL",items))
  })
  output$boxplot <- renderPlot({
    df <- file1data()
    if (is.null(df)) return()
    storenames <- list()
    storeplot  <- list()
    for (i in 1:length(input$x.box)){
      storenames[i] = input$x.box[i]
    }
    for (i in 1:length(storenames)){
      storeplot[[i]] <- ggplot(data=df,aes_string(x=as.character(storenames[i]),
                                                  y=as.character(input$y.box),
                                                  fill=input$factor.box))+
        geom_boxplot(color="black")
      storeplot[[i]] <- storeplot[[i]]
    }
    
    do.call(grid.arrange,c(storeplot,ncol=2))
    
  })
  
  output$x.hist <- renderUI({
    df <- file1data()
    if (is.null(df)) return()
    items=names(df)
    names(items)=items
    selectInput("x.hist","Choose one or more independent(x)",c("NULL",items),multiple = TRUE)
  })
  output$factor.hist <- renderUI({
    df <- file1data() 
    if(is.null(df)) {return()}
    items=names(df)
    names(items)=items
    selectInput("factor.hist","Choose one factor variable",c("NULL",items))
  })
  output$hist <- renderPlot({
    df <- file1data()
    if (is.null(df)) return()
    storenames <- list()
    storeplot  <- list()
    for (i in 1:length(input$x.hist)){
      storenames[i] = input$x.hist[i]
    }
    for (i in 1:length(storenames)){
      storeplot[[i]] <- ggplot(data=df,aes_string(x=as.character(storenames[i]),
                                                  fill=input$factor.hist))+
        geom_histogram(bins = input$bins,color="black")
      storeplot[[i]] <- storeplot[[i]]
    }
    
    do.call(grid.arrange,c(storeplot,ncol=2))
    
  })
  
  #---------------machine learning----------------#
  file_machine <- reactive({
    infile <- input$file_machine
    if (is.null(infile)){return()}
    read.csv(infile$datapath,fileEncoding = 'BIG5')
  })
  output$x.rpart <- renderUI({
    df <- file_machine()
    if (is.null(df)) return()
    items=names(df)
    names(items)=items
    selectInput("x.rpart","Choose one or more independent(x)",c("NULL",items),multiple = TRUE)
  })
  output$y.rpart <- renderUI({
    df <- file_machine()
    if (is.null(file_machine())) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("y.rpart","Choose one dependent(Y)",c("NULL",items))
  })
  output$rpartplot <- renderPlot({
    input$go
    isolate({
      df <- file_machine()
      if (is.null(df)) return()
      tree <- rpart(as.formula(paste(input$y.rpart," ~ ",paste(input$x.rpart,collapse="+"))),data=df,method="class")
      rpart.plot(tree,tweak = 1.3)
    })
  })
  
  output$x.forest <- renderUI({
    df <- file_machine()
    if (is.null(df)) return()
    items=names(df)
    names(items)=items
    selectInput("x.forest","Choose one or more independent(x)",c("NULL",items),multiple = TRUE)
  })
  output$y.forest <- renderUI({
    df <- file_machine()
    if (is.null(file_machine())) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("y.forest","Choose one dependent(Y)",c("NULL",items))
  })
  forest <- reactive({
    input$go.forest
    isolate({
      df <- file_machine()
      if (is.null(df)) return()
      randomForest(as.formula(paste(input$y.forest," ~ ",paste(input$x.forest,collapse="+"))), data = df, importance=TRUE) 
    })
  })
  output$importanceplot <- renderPlot({
    plot(forest())
  })
  output$forestplot <- renderPlot({
    varImpPlot(forest())
  })
  
  #---------------third page----------------#
  output$ui.action <- renderUI({
    if (is.null(fiel1data)) return()
    actionButton("action", "update")
  })
  code <- reactive({
    input$action
    isolate({
      if(input$board != "NULL"){
        NBA<-tibble()
        urlhead <- "https://www.ptt.cc"
        urlmed  <- str_c("/bbs/",input$board,"/index")
        urltail <- ".html"
        url.page <- str_c(urlhead,urlmed,urltail)
        ###add
        if(input$board == "Gossiping"){
          gossiping.url=url.page
          gossiping.session <- html_session(url = gossiping.url)
          
          gossiping.form <- gossiping.session %>%
            html_node("form") %>%
            html_form()
          
          gossiping <- submit_form(
            session = gossiping.session,
            form = gossiping.form,
            submit = "yes"
          )
          gossiping %>%
            html_nodes("a") %>%
            html_attr('href') %>%
            str_subset("index[0-9]{2,}\\.html") %>%
            str_extract('[0-9]+') %>% as.numeric() -> page.latest
        }else{
          html_session(url.page) %>% 
            html_nodes("a") %>%
            html_attr('href') %>%
            str_subset("index[0-9]{2,}\\.html") %>%
            str_extract('[0-9]+') %>% as.numeric() -> page.latest
        }
        
        ####add
        for(i in c(page.latest:(page.latest-input$page+1))){
          #          str_c(urlhead,urlmed,page.latest,urltail) 
          url.html <- str_c(urlhead,urlmed,i,urltail)
          #add
          if(input$board == "Gossiping"){
            uri.target <- gossiping %>% jump_to(url.html)
          }else{
            uri.target<-html_session(url.html)
          }
          #add
          uri.target %>% html_nodes("a")  %>%
            html_attr('href') %>%
            str_subset(str_c("/bbs/",input$board,"/[A-Z].[0-9]{2,}")) -> contenturl
          for(k in c(1:length(contenturl))){
            uri.target %>%
              jump_to(str_c(urlhead,contenturl[k])) %>%
              html_nodes(".article-meta-value") %>% 
              html_text() -> element
            author <- element[1] %>% str_extract("^[A-z0-9_]+") #作者
            board <- element[2]
            title <- element[3]
            date <- element[4]
            content <- uri.target %>%
              jump_to(str_c(urlhead,contenturl[k])) %>% html_nodes( # 內⽂部分
                xpath = '//div[@id="main-content"]/node()[not(self::div|self::span[@class="f2"])]'
              ) %>%
              html_text(trim = TRUE) %>%
              str_c(collapse = "")
            NBA <- NBA %>%
              bind_rows(
                tibble(
                  datetime = date,
                  title = title,
                  author = author,
                  content = content,
                  url = url.html
                ))}}
        return(NBA)
      }
      else{return("Error")}
    })
  })
  output$test <- renderTable({
    head(code())
  })
  
  output$nums <- renderPrint({
    nrow(code())
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste(input$board,".csv",sep="")},
    content = function(file){
      write.csv(code(),file,row.names = FALSE,sep=",",fileEncoding = 'BIG5')
    }
  )
  
  #---------------forth page----------------#
  
  
  
  
})



require(devtools)
require(tableHTML)
require(shinythemes)
require(shiny)
require(shinydashboard)
require(stringr)
require(ggplot2)
require(tidyverse)
require(keras)
require(jsonlite)
require(DT)
require(shinyjs)
require(tidyverse)
require(tidytext)
require(scales)
require(textstem)
require(gridExtra)
require(xtable)
require(topicmodels)

require(rlang)
require(wordcloud)
require(tm)
require(syuzhet)
require(textdata)
require(sentimentr)


##########################################################################



### DEFINE UI



# HEADER
header <- dashboardHeader(
    title = span("TextCraft",style = "font-size:25px"),
    titleWidth = 300
)



#SIDEBAR
sidebar <- dashboardSidebar(sidebarMenuOutput('menu'), width = 160)


# BODY
body <- dashboardBody(
    tags$style(make_css(list('.box',c('font-size'),c('13px')))),
    #shinyDashboardThemes(theme = "grey_dark"),
    tabItems(
        # homepage tab content
        tabItem(tabName="t1",
                fluidRow(
                    box(title= span('Project Introduction',style="font-size:20px"),textOutput("projinfo"),height = 150,width = 12,solidHeader = TRUE,status="primary"))),
        
        ## second tab content
        tabItem(tabName="t2",
                fluidRow(
                    box(title = span('Enter API Link',style="font-size:20px") , textInput("apilink", label = "API", value = "", width = NULL,placeholder = "Enter API Here.."), height = 150,solidHeader = TRUE,width = 4,background = "black" ),
                    #box(title = span('Query',style="font-size:20px"),textOutput("txtOutput1"),height = 150,solidHeader = TRUE,width = 2,   tags$head(tags$style("#txtOutput1{color: orange;font-size: 45px;font-style: bold;}")),background = "black" ),
                    box(title = span('Upload File',style= "font-size:20px"),fileInput("file1", "Choose CSV File",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),height = 150,solidHeader = TRUE,width = 4,background = "black" ),
                    box( actionButton("runif", "Load",style="color: #fff; background-color: #337ab7; border-color: #2e6da4;
                         padding:30px; font-size:170%;margin-left: 40px; margin-top: 20px"),
                         actionButton("reset", "Clear",style="color: #fff; background-color: #337ab7; border-color: #2e6da4;
                         padding:30px; font-size:170%;margin-left: 40px; margin-top: 20px;"),
                         height = 150,solidHeader = TRUE,width = 3,background = "black" )), 
                    
                
                fluidRow(
                    box(title = span('Contents',style = "font-size:20px"),column(width = 12,DT::dataTableOutput("contents"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"), height = 595,solidHeader = TRUE,width = 8,status = "primary"),
                    box(title = span('Select Required Features',style="font-size:20px") , uiOutput("select_corpus"),uiOutput("select_response"), height = 200,solidHeader = TRUE,width = 3,background = "black" ))
                    #box(title = span('Select Response',style="font-size:20px") , uiOutput("select_response"), height = 150,solidHeader = TRUE,width = 2,background = "black" ))
                ),
        
        #third tab 
        tabItem(tabName="t3",
                fluidRow(
                    box(title = span('General Word Frequency',style="font-size:20px"), 
                        splitLayout(cellWidths = c("33.3%", "33.3%","33.3%"), 
                                    div(style = 'overflow-y: scroll;max-height:225px;margin-left: 30px;',tableOutput("overall_wordcount")),
                                    div(style = 'overflow-y: scroll;max-height:225px;margin-left: 30px;',tableOutput("overall_tfidf")),
                                    div(style = 'overflow-y: scroll;max-height:225px;margin-left: 30px;',tableOutput("overall_suggestion"))), 
                        height = 300,solidHeader = TRUE,width = 6,status="primary"),
                    box(title = span('Annotation',style="font-size:20px"),
                        splitLayout(cellWidths = c("30%","30%", "20%","20%"),
                                    uiOutput("select_custom_words",style = "height:225px"),
                                    div(style = 'overflow-y: scroll;max-height:225px;margin-left: 60px;',tableOutput("show_custom_words")),
                                    actionButton("annotate", "Annotate",style="color: #fff; background-color: #337ab7; border-color: #2e6da4;
                                                 padding:20px; font-size:140%;margin-left: 20px; margin-top: 70px"),
                                    actionButton("reset_annotate", "Reset",style="color: #fff; background-color: #337ab7; border-color: #2e6da4;
                                                 padding:20px; font-size:140%;margin-left: 20px; margin-top: 70px")),
                        height = 300,solidHeader = TRUE,width = 6,status ="primary")),
                
                fluidRow(
                    tableOutput("test")
                ),
               
                fluidRow(
                    box(title = span('Configuration',style="font-size:20px"),
                        splitLayout(cellWidths = c("50%","50%"),
                            verticalLayout(uiOutput("select_ngram",style = "height:100px;width: 50%;margin-left: 40px;"),
                                       uiOutput("select_seed",style = "width: 50%;margin-left: 40px;")),
                            div(style = 'max-width:90%;max-height:225px',verticalLayout(uiOutput("select_burning",style = "width: 50%;margin-left: 35px;"),
                                           uiOutput("select_number_topics",style = "margin-top:25px;height:160px;width: 50%;margin-left: 35px;")))),
                        height = 350,solidHeader = TRUE,width = 6,status = "primary"),
                    
                    actionButton("analyze", "Analyze Document Corpus",style="color: #fff; background-color: #7790BF; border-color: #2e6da4;
                                                 padding:40px; font-size:190%;margin-left: 200px; margin-top: 90px")
                    
                    
                    )
   
                ),
                    
        
        #fourth tab
        tabItem(tabName = "t4",
                fluidRow(
                   box(title= span('Select Model',style= "font-size:20px"),
                       selectInput("selected_insights_model", choices = c("Topic Analysis","Sentiment Analysis"),label = ""),
                       solidHeader = TRUE,width = 3,height = 150,background = "black")),
                fluidRow(
                    plotOutput("plot1")),
                fluidRow(
                    splitLayout(cellWidths = c("50%","50%"),
                                plotOutput("plot2"),
                                plotOutput("plot3"))),
                fluidRow(
                    plotOutput("plot4")),
                fluidRow(
                    tableOutput("test1"))
     
                ),
        
        
        #fifth tab
        tabItem(tabName="t5",
                fluidRow(
                    box(title= span('Contact Info',style="font-size:20px"),uiOutput("moreinfo1"),height = 150,width = 12,solidHeader = TRUE,status="primary")))
    ))




ui <- dashboardPage(header,sidebar,body,title = "URAT")



#### DEFINE SERVER

server <- function(input, output){
    
    output$menu<-renderMenu({sidebarMenu(menuItem(text=span('Home',style="font-size:18px"),tabName = 't1'),
                                         menuItem(text= span('Load Data',style="font-size:18px"),tabName = "t2"),
                                         menuItem(text= span('Process Data',style="font-size:18px"),tabName = "t3"),
                                         menuItem(text= span('Get Insights',style="font-size:18px"),tabName = "t4"),
                                         menuItem(text= span('Contact Info',style="font-size:18px"),tabName = "t5") ) })  
    
    output$moreinfo1 <- renderUI({
        HTML(paste("<p>","To get more details on this project, please visit the following link-","<br>"),
             paste("<a href =","https://github.com/neonflux56/Project_EODB_MGTA452",">","Project on Github","</a>"),
             paste("<p>","For further queries and suggestions, please contact through the below link-","<br>"),
             paste("<a href =","https://ashishgupta.netlify.com/",">","Contact","</a>"))
    })
    
    
    output$projinfo <- renderText({
        #t <- print(source("Get_API_Response.R")[["value"]])
        #datatable(t)
        "Welcome to the FUTURE!"
        
    })
    
    
    #LOAD RESET reactive
    v <- reactiveValues(data = NULL)
    
    # LOAD DATA BUTTON
    observeEvent(input$runif, {
        api <- input$apilink
        inFile <- input$file1
        if(is.null(inFile) && api == "") {
            v$data <- NULL
        }
        else{
            v$data <- 1
        }
    })
    
    
    
    # RESET DATA BUTTON
    observeEvent(input$reset, {
        v$data <- NULL
        reset("apilink")
        reset("file1")
    })  
    

    
    
    # GET MAIN DATA 
    table_out <- reactive({
        api <- input$apilink
        inFile <- input$file1
        if (is.null(v$data)){
            return(NULL)
        }
        if (api == "" && v$data == 1){
            table_out <- read.csv(inFile$datapath)
        }
        else {
            source("Get_API_Response.R")
            table_out <- test_api(api)
        }
        return(table_out)
    })
    
    
    
    
    # SHOW CONTENTS DATA TABLE
    output$contents <- renderDataTable({
        datatable(table_out(), options = list(paging = FALSE))
    })
    
    
    # SELECT CORPUS COLUMN
    output$select_corpus <- renderUI({
        selectInput("selected_corpus",  choices = colnames(table_out()), label = "Document Corpus")
    })
    
    
    # SELECT RESPONSE COLUMN
    output$select_response <- renderUI({
        selectInput("selected_response", choices = colnames(table_out()), label = "Feature Response")
    })

    
    
    
    #### RUN REMOVED COLUMNS ON DATA
    
    #ANNOTATE RESET reactive
    a <- reactiveValues(data = NULL)
    words_to_remove <- reactiveValues(data = NULL)
    
    # ANNOTATE DATA BUTTON
    observeEvent(input$annotate, {
        if (length(selected_remove_custom_words) != 0){
            a$data <- 1
            words_to_remove1 <- input$selected_remove_custom_words
            words_to_remove$data <- append(words_to_remove$data ,words_to_remove1)
        }
    })
    
    #RESET ANNOTATE BUTTON
    observeEvent(input$reset_annotate, {
        a$data <- NULL
    })
    
    # GET USER DEFINED WORDS
    selected_remove_custom_words <- reactive({
        if (is.null(a$data)){
            return(c())
        }
        else{
            return(words_to_remove$data)
        }
    }) 
    
   
    # SHOW TOP WORDCOUNT DATAFRAME
    output$overall_wordcount <- renderTable({
        corpus_name <- input$selected_corpus
        response_name <- input$selected_response
        selected_remove_custom_words <- selected_remove_custom_words()
        df <- data.frame(table_out())
        df <- df %>% select(corpus_name,response_name)
        source("Exploratory_Analysis.R")
        selected_remove_custom_words <- selected_remove_custom_words()
        df <- get_overall_wordcount(df,as.character(corpus_name),as.character(response_name),selected_remove_custom_words)
        df
        },caption = "Top 10 word counts",caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    
    # SHOW TOP TFIDF DATAFRAME
    output$overall_tfidf <- renderTable({
        corpus_name <- input$selected_corpus
        response_name <- input$selected_response
        selected_remove_custom_words <- selected_remove_custom_words()
        df <- data.frame(table_out())
        df <- df %>% select(corpus_name,response_name)
        source("Exploratory_Analysis.R")
        selected_remove_custom_words <- selected_remove_custom_words()
        df <- get_overall_wordcount(df,as.character(corpus_name),as.character(response_name),selected_remove_custom_words)
        df
    },caption = "Top 10 word tf-idf scores",caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    

    
    # SHOW SUGGESTION DATAFRAME
    output$overall_suggestion <- renderTable({
        corpus_name <- input$selected_corpus
        response_name <- input$selected_response
        selected_remove_custom_words <- selected_remove_custom_words()
        df <- data.frame(table_out())
        df <- df %>% select(corpus_name,response_name)
        source("Exploratory_Analysis.R")
        df <- get_overall_suggestion(df,as.character(corpus_name),as.character(response_name),selected_remove_custom_words)
        df
    }, caption = "Suggested words to remove",caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    
    
    # USER REMOVE WORDS DROPDOWN
    output$select_custom_words <- renderUI({
        corpus_name <- input$selected_corpus
        response_name <- input$selected_response
        selected_remove_custom_words <- selected_remove_custom_words()
        df <- data.frame(table_out())
        df <- df %>% select(corpus_name,response_name)
        source("Exploratory_Analysis.R")
        df <- get_overall_suggestion(df,as.character(corpus_name),as.character(response_name),selected_remove_custom_words)
        selectInput("selected_remove_custom_words", choices = df$word ,multiple = TRUE, label = "Select the words to remove")
    })
    
    
    # SHOW USER REMOVED WORDS
    output$show_custom_words <- renderTable({
        selected_remove_custom_words()
    }, caption = "Words removed",caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    
    
    #GET FINAL CLEAN DATA
    Clean_data <- reactive({
        corpus_name <- input$selected_corpus
        response_name <- input$selected_response
        selected_remove_custom_words <- selected_remove_custom_words()
        df <- data.frame(table_out())
        df <- df %>% select(corpus_name,response_name)
        source("Exploratory_Analysis.R")
        df <- get_clean_data(df,as.character(corpus_name),as.character(response_name),selected_remove_custom_words)
        return(df)
    })
        
    
    
    #TEST CLEAN DATA
    output$test <- renderTable({
        head(Clean_data())
    })
    
    
    
    # SELECT CONFIGURATION NGRAM COLUMN
    output$select_ngram <- renderUI({
        selectInput("selected_ngram", choices = c(1,2), label = "Select ngram type")
    })
    
    
    # SELECT CONFIGURATION NGRAM COLUMN
    output$select_seed <- renderUI({
        textInput("selected_seed", label = "Enter seed value", value = 1234)
    })
    
    # SELECT CONFIGURATION NGRAM COLUMN
    output$select_burning <- renderUI({
        textInput("selected_burning", label = "Select burning ratio" , value = 1)
    })
    
    # SELECT CONFIGURATION NGRAM COLUMN
    output$select_number_topics <- renderUI({
        selectInput("selected_number_topics", choices = seq(2,25,1), label = "Choose number of topics")
    })
    
    
    
    
    # RUN LDA REACTIVE
    Run_LDA <- reactive({
        selected_ngram <- input$selected_ngram
        selected_seed <- input$selected_seed
        selected_burning <- input$selected_burning
        selected_number_topics <- input$selected_number_topics
        corpus_name <- input$selected_corpus
        response_name <- input$selected_response
        clean_data <- Clean_data()
        source("Model_Analyze.R")
        lda_result <- run_lda(clean_data,as.character(corpus_name),as.character(response_name),as.numeric(selected_ngram),
                              as.numeric(selected_seed),as.numeric(selected_burning),as.numeric(selected_number_topics))
        return(lda_result)
    })
    
    
    
    # RUN SENTIMENT REACTIVE
    Run_Sentiment <- reactive({
        selected_ngram <- input$selected_ngram
        selected_seed <- input$selected_seed
        selected_burning <- input$selected_burning
        selected_number_topics <- input$selected_number_topics
        corpus_name <- input$selected_corpus
        response_name <- input$selected_response
        clean_data <- Clean_data()
        source("Model_Analyze.R")
        sentiment_result <- run_sentiment(clean_data,as.character(corpus_name),as.character(response_name),as.numeric(selected_ngram),
                              as.numeric(selected_seed),as.numeric(selected_burning),as.numeric(selected_number_topics))
        return(sentiment_result)
    })
    
    
    # INITIALIZE MODEL RESULTS 
    lda_result <- reactiveValues()
    sentiment_result <- reactiveValues()
    
    
    
    # RUN MODELLING AND ANALYZE TEXT ON CLICK
    observeEvent(input$analyze, {
        #LDA Results
        #lda_result$data <- Run_LDA()[[1]]
        lda_result$plot <- Run_LDA()[[2]]
        
        #Sentiment Results
        #sentiment_result$test1 <- Run_Sentiment()[[1]]
        #sentiment_result$plot1 <- Run_Sentiment()[[1]]
        sentiment_result$plot2 <- Run_Sentiment()[[2]]
        sentiment_result$plot3 <- Run_Sentiment()[[3]]
        sentiment_result$plot4 <- Run_Sentiment()[[4]]
    })
    
    
 
    ###################################### INSIGHTS TAB
    

    
    # PLOT-1 
    output$plot1 <- renderPlot({
        selected_insights_model <- input$selected_insights_model
        
        if (selected_insights_model == "Topic Analysis"){
            lda_result$plot
        }
        else{
            sentiment_result$plot3
        }
    })
    
    
    
    # PLOT-2
    output$plot2 <- renderPlot({
        selected_insights_model <- input$selected_insights_model
        
        if (selected_insights_model == "Topic Analysis"){
            
        }
        else{
            sentiment_result$plot2
        }
    })
    
    
    # PLOT-3
    output$plot3 <- renderPlot({
        selected_insights_model <- input$selected_insights_model
        
        if (selected_insights_model == "Topic Analysis"){
            
        }
        else{
            sentiment_result$plot4
        }
    })
    
    
    # PLOT-4
    output$plot4 <- renderPlot({
        selected_insights_model <- input$selected_insights_model
        
        if (selected_insights_model == "Topic Analysis"){
            
        }
        else{
            #sentiment_result$plot4
        }
    })
    
    output$test1 <- renderTable(({
        #sentiment_result$test1
    }))
    
    
    
    
    
}





shinyApp(ui,server)




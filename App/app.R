

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
require(ggridges)
require(rlang)
require(wordcloud)
require(wordcloud2)
require(tm)
require(syuzhet)
require(textdata)
require(sentimentr)
require(shinycssloaders)


##########################################################################



### DEFINE UI

#Loading feature design
options(spinner.color="#add8e6", spinner.color.background="#ffffff", spinner.size=1,spinner.type = 1)

# HEADER
header <- dashboardHeader(
    title = span("TextCraft",style = "font-size:25px; font-family: serif;color:#191970; font-weight: bold"),
    titleWidth = 300
)



#SIDEBAR
sidebar <- dashboardSidebar(
    
    br(),
    
    tags$style(HTML('
                  .main-sidebar{
                  font-family: "serif";
                  font-weight: bold;
                  font-size: 15px;
                  color: blue;
                  }
                  .skin-black .sidebar-menu > li.active > a {
                  background-color: #F0F8FF;
                  }
                  .skin-black .sidebar-menu > li:hover > a {
                  background-color: #F0F8FF;
                  }
                  .skin-black .main-sidebar .sidebar .sidebar-menu a{
                          color: #191970;
                          font-weight: bold;
                  }
                  .h5 {
                          color: #191970
                          }
                  '
                    
    )),
    
    
    
    sidebarMenuOutput('menu'),
    width = 160)


# BODY
body <- dashboardBody(
    tags$style(HTML("
          .content-wrapper {
            background-color: white; !important;
          }
          .main-sidebar {
            background-color: white !important;
           
          }
        "
    ))
    , 
    tabItems(
        # homepage tab content
        tabItem(tabName="t1",
                fluidRow(
                    box(title= span('Project Introduction',style="font-size:20px;"),
                        style = "background-color:#F0F8FF; height:120px; border-top-style: solid; border-top-width: 4px; border-top-color:#191970",
                        textOutput("projinfo"),height = 120,width = 12,solidHeader = TRUE))),
        
        ## second tab content
        tabItem(tabName="t2",
                
                fluidRow(
                    box(#title = span('Enter API Link',style="font-size:20px;") ,
                        style = "background-color:#F0F8FF; height:120px; border-top-style: solid; border-top-width: 4px; border-top-color:#191970" ,
                        textInput("apilink", label = "API", value = "", width = NULL,placeholder = "Enter API Here.."), 
                        height = 120,solidHeader = TRUE,width = 4),
                    box(#title = span('Upload File',style= "font-size:20px"),
                        style = "background-color:#F0F8FF;height:120px; border-top-style: solid; border-top-width: 4px; border-top-color:#191970",
                        fileInput("file1", "Choose CSV File",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                        height = 120,solidHeader = TRUE,width = 4),
                    box(style = "background-color:#F0F8FF;height:120px; border-top-style: solid; border-top-width: 4px; border-top-color:#191970",
                        actionButton("runif", "Load",style="color: #fff; background-color: #191970; border-color: #708090;
                         padding:20px; font-size:120%;margin-left: 40px; margin-top: 10px"),
                        actionButton("reset", "Clear",style="color: #fff; background-color: #191970; border-color: #708090;
                         padding:20px; font-size:120%;margin-left:20px; margin-top: 10px;"),
                        height = 120,solidHeader = TRUE,width = 3)), 
                
                
                fluidRow(
                    box(title = span('Contents',style = "font-size:20px"),style = "background-color:#F0F8FF;height:555px; border-top-style: solid; border-top-width: 4px; border-top-color:#191970",
                        column(width = 12,withSpinner(DT::dataTableOutput("contents")),
                               style = "height:500px; overflow-y: scroll;overflow-x: scroll;"), 
                        height = 595,solidHeader = TRUE,width = 8),
                    box(title = span('Select Required Features',style="font-size:20px"), 
                        style = "background-color:#F0F8FF;height:180px; border-top-style: solid; border-top-width: 4px; border-top-color:#191970",
                        uiOutput("select_corpus"),
                        uiOutput("select_response"), 
                        height = 220,solidHeader = TRUE,width = 3))
                  ),
        
        
        #third tab 
        tabItem(tabName="t3",
                fluidRow(
                    box(title = span('Top 10 Word Counts',style="font-size:20px"), 
                        style = "background-color:#F0F8FF; height:260px; border-top-style: solid; border-top-width: 4px; border-top-color:#191970",
                        div(style = 'overflow-y: scroll;max-height:225px;display:inline-block;width:100%;margin-left:40px;',
                            withSpinner(tableOutput("overall_wordcount"))),
                        height = 300,solidHeader = TRUE,width = 4),
                    box(title = span('Top 10 TF-IDF scores ',style="font-size:20px"), 
                        style = "background-color:#F0F8FF; height:260px; border-top-style: solid; border-top-width: 4px; border-top-color:#191970",
                        div(style = 'overflow-y: scroll;max-height:225px;display:inline-block;width:100%;margin-left:40px;',
                            withSpinner(tableOutput("overall_tfidf"))),
                       height = 300,solidHeader = TRUE,width = 4),
                    box(title = span('Suggested Words to Remove',style="font-size:20px"), 
                        style = "background-color:#F0F8FF; height:260px; border-top-style: solid; border-top-width: 4px; border-top-color:#191970",
                        div(style = 'overflow-y: scroll;max-height:225px;display:inline-block;width:100%;margin-left:40px;',
                            withSpinner(tableOutput("overall_suggestion"))), 
                        height = 300,solidHeader = TRUE,width = 4)),
                
                fluidRow(
                    box(title = span('Annotation',style="font-size:20px"),
                        style = "background-color:#F0F8FF; height:260px; border-top-style: solid; border-top-width: 4px; border-top-color:#191970",
                        splitLayout(cellWidths = c("25%","30%", "25%","10%","10%"),
                                    uiOutput("select_custom_words",style = "height:225px"),
                                    uiOutput("enter_custom_words",style = "height:225px"),
                                    div(style = 'overflow-y: scroll;overflow-x: hidden ;max-height:225px;margin-left :40px',tableOutput("show_custom_words")),
                                    actionButton("annotate", "Annotate",style="color: #fff; background-color: #191970; border-color: #708090;
                                                 padding:10px; font-size:120%;margin-left: 30px; margin-top: 70px"),
                                    actionButton("reset_annotate", "Reset",style="color: #fff; background-color: #191970; border-color: #708090;
                                                 padding:10px; font-size:120%;margin-left: 10px; margin-top: 70px")),
                        height = 300,solidHeader = TRUE,width = 12)),
                
         
                fluidRow(
                    box(title = span('Configuration',style="font-size:20px"),
                        style = "background-color:#F0F8FF; height:310px; border-top-style: solid; border-top-width: 4px; border-top-color:#191970; overflow-x: hidden ;",
                        splitLayout(cellWidths = c("50%","50%"),
                                    div(style = 'max-width:90%;max-height:225px: overflow-x: hidden ;',
                                        verticalLayout(uiOutput("select_ngram",style = "height:100px;width: 50%;margin-left: 40px;"),
                                                       uiOutput("select_seed",style = "width: 50%;margin-left: 40px;"))),
                                    div(style = 'max-width:90%;max-height:225px: overflow-x: hidden ;',
                                        verticalLayout(uiOutput("select_number_words",style = "width: 50%;margin-left: 35px;"),
                                                       uiOutput("select_number_topics",style = "margin-top:25px;height:160px;width: 50%;margin-left: 35px;")))),
                        height = 350,solidHeader = TRUE,width = 12),
                    div(style="display:inline-block;width:100%;text-align: center;",
                        actionButton("analyze", "Analyze Document Corpus",style="color: #fff; background-color: #191970; border-color: #708090;
                                                 padding:20px; font-size:150%;")),br(),br())
                
        ),
        
        
        #fourth tab
        tabItem(
            tabName = "t4",
            tabsetPanel(
                tabPanel("Topic Analysis",
                         
                         fluidRow(
                             br(),
                             column(12, align = "center",withSpinner(plotOutput("t_plot1", width = "60%")),br()))),
                
                tabPanel("Sentiment Analysis",
                         fluidRow(
                             br(),
                             column(12, align = "center",withSpinner(plotOutput("s_plot1", width = "60%")),br())),
                         fluidRow(
                             column(12, align = "center",uiOutput("plotsfor_s_plot2"),br())),
                         fluidRow(
                             column(12, align = "center",withSpinner(plotOutput("s_plot3", width = "60%")),br())),
                         fluidRow(
                             column(12, align = "center",withSpinner(plotOutput("s_plot4", width = "60%")),br())),
                         fluidRow(
                             column(12, align = "center",withSpinner(plotOutput("s_plot5", width = "60%")),br())),
                         fluidRow(
                             column(12, align = "center",withSpinner(plotOutput("s_plot6", width = "60%")),br()))
                ))
        ),
        
        
   
        
        
        #fifth tab
        tabItem(tabName="t5",
                fluidRow(
                    box(title= span('Contact Info',style="font-size:20px"), style = "background-color:#F0F8FF; height:120px; border-top-style: solid; border-top-width: 4px; border-top-color:#191970",
                        uiOutput("moreinfo1"),height = 150,width = 12,solidHeader = TRUE)))
    ))




ui <- dashboardPage(skin = "black",header,sidebar,body,title = "TextCraft")



#### DEFINE SERVER

server <- function(input, output){
    
    
    output$menu<-renderMenu({sidebarMenu(menuItem(text=span('Home',style="font-size:18px"),tabName = 't1', icon= icon("couch")),
                                         menuItem(text= span('Load Data',style="font-size:18px"),tabName = "t2", icon= icon("refresh")),
                                         menuItem(text= span('Process Data',style="font-size:18px"),tabName = "t3",icon=icon("desktop")),
                                         menuItem(text= span('Get Insights',style="font-size:18px"),tabName = "t4", icon=icon("info")),
                                         menuItem(text= span('Contact Info',style="font-size:18px"),tabName = "t5", icon= icon("address-book")) ) })   
    
    output$moreinfo1 <- renderUI({
        HTML(paste("<p>","To get more details on this project, please visit the following link-","<br>"),
             paste("<a href =","https://github.com/neonflux56/Project_EODB_MGTA452",">","Project on Github","</a>"),
             paste("<p>","For further queries and suggestions, please contact through the below link-","<br>"),
             paste("<a href =","https://ashishgupta.netlify.com/",">","Contact","</a>"))
    })
    
    
    output$projinfo <- renderText({
        "Welcome to the FUTURE! Conduct your own topic model and sentiment analysis. 
          Proceed to the next tab to get started!"
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
    
    
    # GET NUMBER OF RESPONSES
    number_of_responses <- reactive({
        corpus_name <- input$selected_corpus
        response_name <- input$selected_response
        df <- data.frame(table_out())
        df <- df %>% select(corpus_name,response_name)
        source("Exploratory_Analysis.R")
        n <- get_number_responses(df,as.character(response_name))
        return(n)
    })
    
    
    
    
    #TEST 
    #output$test <- renderText({
    #    number_of_responses()
    #})
    
    
    
    
    
    
    #### RUN REMOVED COLUMNS ON DATA
    
    #ANNOTATE RESET reactive
    a <- reactiveValues(data = NULL)
    words_to_remove <- reactiveValues(data = NULL)
    
    # ANNOTATE DATA BUTTON
    observeEvent(input$annotate, {
        if (length(input$selected_remove_custom_words) != 0 | input$entered_custom_words != "" ){
            a$data <- 1
            words_to_remove1 <- input$selected_remove_custom_words
            words_to_remove2 <- unlist(strsplit(as.character(input$entered_custom_words),",",fixed=TRUE))
            words_to_remove$data <- append(words_to_remove$data ,words_to_remove1)
            words_to_remove$data <- append(words_to_remove$data ,words_to_remove2)
            
        }
    })
    
    #RESET ANNOTATE BUTTON
    observeEvent(input$reset_annotate, {
        a$data <- NULL
        words_to_remove$data <- NULL
    })
    
    # GET USER DEFINED SELECTED WORDS
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
    })
    
    
    # SHOW TOP TFIDF DATAFRAME
    output$overall_tfidf <- renderTable({
        corpus_name <- input$selected_corpus
        response_name <- input$selected_response
        selected_remove_custom_words <- selected_remove_custom_words()
        df <- data.frame(table_out())
        df <- df %>% select(corpus_name,response_name)
        source("Exploratory_Analysis.R")
        selected_remove_custom_words <- selected_remove_custom_words()
        df <- get_overall_tfidf(df,as.character(corpus_name),as.character(response_name),selected_remove_custom_words)
        df
    })
    
    
    
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
    })
    
    
    
    # USER REMOVE WORDS DROPDOWN
    output$select_custom_words <- renderUI({
        corpus_name <- input$selected_corpus
        response_name <- input$selected_response
        selected_remove_custom_words <- selected_remove_custom_words()
        df <- data.frame(table_out())
        df <- df %>% select(corpus_name,response_name)
        source("Exploratory_Analysis.R")
        df <- get_overall_suggestion(df,as.character(corpus_name),as.character(response_name),selected_remove_custom_words)
        selectInput("selected_remove_custom_words", choices = df$Word ,multiple = TRUE, label = "Select the words to remove")
    })
    
    # USER REMOVE CUSTOM WORDS
    output$enter_custom_words <- renderUI({
        textInput("entered_custom_words", label = "Enter custom words to remove (Separate by commas)")
    })
    
    
    # SHOW USER REMOVED WORDS
    output$show_custom_words <- renderTable({
        data.frame( Word = selected_remove_custom_words())
    }, caption = "Words Removed",caption.placement = getOption("xtable.caption.placement", "top"), 
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
    
    
    
    
    
    
    
    # SELECT CONFIGURATION NGRAM COLUMN
    output$select_ngram <- renderUI({
        selectInput("selected_ngram", choices = c(1,2), label = "Select ngram type")
    })
    
    # SELECT CONFIGURATION SEED COLUMN
    output$select_seed <- renderUI({
        textInput("selected_seed", label = "Enter seed value", value = 1234)
    })
    
    # SELECT CONFIGURATION NUMBER OF WORDS COLUMN
    output$select_number_words <- renderUI({
        textInput("selected_number_words", label = "Enter number of top words" , value = 10)
    })
    
    # SELECT CONFIGURATION NUMBER OF TOPICS COLUMN
    output$select_number_topics <- renderUI({
        selectInput("selected_number_topics", choices = seq(2,25,1), label = "Choose number of topics")
    })
    
    
    
    
    # RUN LDA REACTIVE
    Run_LDA <- reactive({
        
        selected_ngram <- input$selected_ngram
        selected_seed <- input$selected_seed
        selected_number_words <- input$selected_number_words
        selected_number_topics <- input$selected_number_topics
        corpus_name <- input$selected_corpus
        response_name <- input$selected_response
        clean_data <- Clean_data()
        source("Model_Analyze.R")
        lda_result <- run_lda(clean_data,as.character(corpus_name),as.character(response_name),as.numeric(selected_ngram),
                              as.numeric(selected_seed),as.numeric(selected_number_words),as.numeric(selected_number_topics))
        return(lda_result)
        
        
    })
    
    
    
    # RUN SENTIMENT REACTIVE
    Run_Sentiment <- reactive({
        selected_ngram <- input$selected_ngram
        selected_seed <- input$selected_seed
        selected_number_words <- input$selected_number_words
        selected_number_topics <- input$selected_number_topics
        corpus_name <- input$selected_corpus
        response_name <- input$selected_response
        clean_data <- Clean_data()
        source("Model_Analyze.R")
        sentiment_result <- run_sentiment(clean_data,as.character(corpus_name),as.character(response_name),as.numeric(selected_ngram),
                                          as.numeric(selected_seed),as.numeric(selected_number_words),as.numeric(selected_number_topics))
        return(sentiment_result)
        
    })
    
    
    # INITIALIZE MODEL RESULTS 
    lda_result <- reactiveValues()
    sentiment_result <- reactiveValues()
    
    
    
    # RUN MODELLING AND ANALYZE TEXT ON CLICK
    observeEvent(input$analyze, {
        
        
        isolate({
            withProgress({
                
                setProgress(message = "Processing corpus...")
                
                #LDA Results
                lda_result$t_plot1 <- Run_LDA()[[1]]
                
                #Sentiment Results
                sentiment_result$s_plot1 <- Run_Sentiment()[[1]]
                sentiment_result$s_plot2 <- Run_Sentiment()[[2]]
                sentiment_result$s_plot3 <- Run_Sentiment()[[3]]
                sentiment_result$s_plot4 <- Run_Sentiment()[[4]]
                sentiment_result$s_plot5 <- Run_Sentiment()[[5]]
                sentiment_result$s_plot6 <- Run_Sentiment()[[6]]
                
                ###################################### INSIGHTS TAB
                
                # PLOT-1 
                output$t_plot1 <- renderPlot({
                    lda_result$t_plot1
                })
                output$s_plot1 <- renderPlot({
                    sentiment_result$s_plot1
                })
                
                
                
                # PLOT-2
                ###### Dynamic
                output$plotsfor_s_plot2 <- renderUI({
                    plot_output_list <- lapply(1:as.numeric(number_of_responses()), function(i) {
                        plotname <- paste("plot", i, sep="")
                        withSpinner(plotOutput(plotname, width = "60%")) 
                    })
                    do.call(tagList, plot_output_list)
                })
                for (i in 1:as.numeric(number_of_responses())) {
                    local({
                        my_i <- i
                        plotname <- paste("plot", my_i, sep="")
                        output[[plotname]] <- renderPlot({
                            sentiment_result$s_plot2[[my_i]]
                        })
                    })
                }
                
                
                
                # PLOT-3
                output$s_plot3 <- renderPlot({
                    sentiment_result$s_plot3
                })
                
                
                # PLOT-4
                output$s_plot4 <- renderPlot({
                    sentiment_result$s_plot4
                })
                
                # PLOT-5
                output$s_plot5 <- renderPlot({
                    sentiment_result$s_plot5
                })
                
                # PLOT-6
                output$s_plot6 <- renderPlot({
                    sentiment_result$s_plot6
                })
                
                
            })
        })
        
    })
    
    
    
}




shinyApp(ui,server)




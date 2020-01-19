

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


#Fetch Data

NYTIMES_KEY="bivqAnNUnxs6CmliCOFeAbuMBIYVPHdn"


# Let's set some parameters
term <- "central+park+jogger" # Need to use + to string together separate words

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")


initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 


pages <- list()
#6 pages
for(i in 0:5){
    nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
    message("Retrieving page ", i)
    pages[[i+1]] <- nytSearch 
    Sys.sleep(1) 
}


allNYTSearch <- rbind_pages(pages)

allNYTSearch[[6]][1]

###############################


















##########################################################################




header <- dashboardHeader(
    title = span("URAT",style = "font-size:25px"),
    titleWidth = 300
)

sidebar <- dashboardSidebar(sidebarMenuOutput('menu'), width = 160)


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
                    box(title = span('Enter API link',style="font-size:20px") , textInput("apilink", label = "API", value = "", width = NULL,placeholder = "Enter API Here.."), height = 150,solidHeader = TRUE,width = 2,background = "black" ),
                    box(title = span('Query',style="font-size:20px"),textOutput("txtOutput1"),height = 150,solidHeader = TRUE,width = 2,   tags$head(tags$style("#txtOutput1{color: orange;font-size: 45px;font-style: bold;}")),background = "black" ))),
                    
        
        #third tab 
        tabItem(tabName="t3",
                fluidRow(
                    box(title = span('Topic',style="font-size:20px"), textInput("topic",label = ""), height = 150,solidHeader = TRUE,width = 2,background = "black"),
                    box(title = span('Topic1',style="font-size:20px"),textOutput("txtOutput2"),height = 150,solidHeader = TRUE,width = 2,   tags$head(tags$style("#txtOutput2{color: orange;font-size: 30px;font-style: bold;}")),background ="black"))),
                    
        #fourth
        tabItem(tabName="t4",
                fluidRow(
                    box(title= span('Contact Info',style="font-size:20px"),uiOutput("moreinfo1"),height = 150,width = 12,solidHeader = TRUE,status="primary")))
        
        
        
    ))


ui <- dashboardPage(header,sidebar,body,title = "URAT")


server <- function(input, output){
    
    output$menu<-renderMenu({sidebarMenu(menuItem(text=span('Home',style="font-size:18px"),tabName = 't1'),menuItem(text= span('API Call',style="font-size:18px"),tabName = "t2"),menuItem(text= span('Initiate Action',style="font-size:18px"),tabName = "t3"),menuItem(text= span('Contact Info',style="font-size:18px"),tabName = "t4") ) })  
    
    output$moreinfo1 <- renderUI({
        HTML(paste("<p>","To get more details on this project, please visit the following link-","<br>"),
             paste("<a href =","https://github.com/neonflux56/Project_EODB_MGTA452",">","Project on Github","</a>"),
             paste("<p>","For further queries and suggestions, please contact through the below link-","<br>"),
             paste("<a href =","https://ashishgupta.netlify.com/",">","Contact","</a>"))
    })
    
    
    output$projinfo <- renderText({
        "hi"
    })
    
}

shinyApp(ui,server)




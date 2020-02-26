#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shiny)

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
        tags$head(
            tags$style(HTML("
          .content-wrapper {
            background-color: linen !important;
          }
          .main-sidebar {
            background-color: powderblue !important;
          }
        "))
        )
    )
)

server <- function(input, output, session) { }

shinyApp(ui, server)
shinyApp(ui = ui, server = server)



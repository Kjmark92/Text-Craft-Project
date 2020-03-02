#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#header

#messages

header <- dashboardHeader(
    dropdownMenu(
        type = "messages",
        messageItem(
            from = "Lucy",
            message = "You can view the International Space Station!",
            href = "https://spotthestation.nasa.gov/sightings/"
        ),
        # Add a second messageItem() 
        messageItem(
            from = "Lucy",
            message = "Learn more about the International Space Station",
            href = "https://spotthestation.nasa.gov/faq.cfm"
        )
    )
)


#Notification Menu

header2 <- dashboardHeader(
    dropdownMenu(
        type = "notifications",
        notificationItem(
            text = "Check out datacamp!",
            href = "http://www.datacamp.com"
        )
    )
)

#Task

header3 <- dashboardHeader(
    dropdownMenu(
        type = "tasks",
        taskItem(
            text = "Look at your datacamp progress!",
            value = 15
        )
    )
)


sidebar <- dashboardSidebar()

body <- dashboardBody()

ui <- dashboardPage(header=header, sidebar=sidebar, body=body)

server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)

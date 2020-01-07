library(shiny)
library(kolejkeR)
library(shinycssloaders)
library(shinyhelper)

offices <- kolejkeR::get_available_offices()

ui <- fluidPage(
    
    titlePanel("kolejkeR"),
    
    sidebarLayout(
        sidebarPanel(
            helper(selectInput("of",
                        "Select office",
                        choices = offices)),
            #conditionalPanel(condition = "typeof input.of === 'undefined' || input.of == null", uiOutput("Queue"))
            uiOutput("Queue")
        ),
        mainPanel(
            helper(textOutput("result1")),
            textOutput("result2"),
            textOutput("result3"),
        
        #    tabsetPanel(type = "tabs",
        #                tabPanel("Plot", 
        #                    helper(textOutput("result1")),
        #                    textOutput("result2"),
        #                    textOutput("result3")),
        #                tabPanel("Summary", helper(textOutput("result1"))),
        #                tabPanel("Table", helper(textOutput("result1")))
        #    )
        )
    )
)



server <- function(input, output, session) {
    
#    observe({
#        validate({need(input$of,"Select office")})
#        input$of
#        output$Queue <- renderUI({
#            selectInput("queue", "Select available queue", choices = kolejkeR::get_available_queues(input$of))
#        })
#    })
    output$Queue <- renderUI({
        selectInput("queue", "Select available queue", choices = kolejkeR::get_available_queues(input$of))
    })
    observe({
        validate({
            need(input$of, "Select office")
            need(input$queue, "Select queue")
        })
        input$of
        output$result1 <- renderText(kolejkeR::get_current_ticket_number_verbose(input$of,input$queue))
        output$result2 <- renderText(kolejkeR::get_number_of_people_verbose(input$of,input$queue))
        output$result3 <- renderText(kolejkeR::get_waiting_time_verbose(input$of,input$queue))
    }
    )
}

shinyApp(ui = ui, server = server)

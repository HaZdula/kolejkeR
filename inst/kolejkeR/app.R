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
            #htmlOutput("Queue"),
            helper(selectInput("queue", "Select available queue", choices = "")),
            actionButton("submit", label = "Print results")
        ),
        mainPanel(
            # helper(textOutput("result1")),
            # textOutput("result2"),
            # textOutput("result3")
            tabsetPanel(
                        tabPanel("Plot",
                            textOutput("result1"),
                            textOutput("result2"),
                            textOutput("result3")),
                        tabPanel("Summary", textOutput("result4")),
                        tabPanel("Table", textOutput("result5"))
            )
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
    observe({
        updateSelectInput(session, "queue", "Select available queue", choices = kolejkeR::get_available_queues(input$of))
    })
    # observe({
    #     validate({
    #         need(input$of, "Select office")
    #         need(input$queue, "Select queue")
    #         need(input$submit, label = "Click the button")
    #     })
    results <- reactiveValues(res1="", res2="", res3="")
    observeEvent(input$submit,{
        # input$of
        results$res1 <- kolejkeR::get_current_ticket_number_verbose(input$of,input$queue)
        results$res2 <- kolejkeR::get_number_of_people_verbose(input$of,input$queue)
        results$res3 <- kolejkeR::get_waiting_time_verbose(input$of,input$queue)
    }
    )
    output$result1 <- renderText(results$res1)
    output$result2 <- renderText(results$res2)
    output$result3 <- renderText(results$res3)
    output$result4 <- renderText("Summary")
    output$result5 <- renderText("Table")
}

shinyApp(ui = ui, server = server)

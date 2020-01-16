library(shiny)
library(kolejkeR)
library(shinycssloaders)
library(shinyhelper)
library(DT)
offices <- kolejkeR::get_available_offices()

# mozna brac jezyk z przegladarki badz dac przycisk dla usera
# https://stackoverflow.com/questions/47750273/shiny-application-get-browser-language-settings
library(shiny.i18n)
i18n <- Translator$new(translation_json_path = "../raw/translation.json")
lang <- "pl"
i18n$set_translation_language(lang)

ui <- fluidPage(
    
    titlePanel("kolejkeR"),
    
    sidebarLayout(
        sidebarPanel(
            helper(selectInput("of",
                        i18n$t("Select office"),
                        choices = offices)),
            #conditionalPanel(condition = "typeof input.of === 'undefined' || input.of == null", uiOutput("Queue"))
            # dzięki selectizeInput mamy autocomplete, ale trzeba obsłużyć brzydkie inputy
            helper(selectizeInput("queue", i18n$t("Select available queue"), choices = "")),
            actionButton("submit", label = i18n$t("Print results"))
        ),
        mainPanel(
            # helper(textOutput("result1")),
            # textOutput("result2"),
            # textOutput("result3")
            tabsetPanel(
                        tabPanel(i18n$t("Current state"),
                            textOutput("result1"),
                            textOutput("result2"),
                            textOutput("result3")),
                        tabPanel(i18n$t("Table"), DTOutput("result4")),
                        tabPanel(i18n$t("Predictions"), textOutput("result5"))
                        )
        )
    )
)

server <- function(input, output, session) {
    choices <- reactiveVal()

    observe({
        choices(kolejkeR::get_available_queues(input$of))
        updateSelectizeInput(session, "queue", i18n$t("Select available queue"), 
                          choices = choices())
    })

    results <- reactiveValues(res1="", res2="", res3="")
    observeEvent(input$submit,{
        if(!input$queue %in% choices()) return()
        results$res1 <- kolejkeR::get_current_ticket_number_verbose(input$of,input$queue, language = lang)
        results$res2 <- kolejkeR::get_number_of_people_verbose(input$of,input$queue, language = lang)
        results$res3 <- kolejkeR::get_waiting_time_verbose(input$of,input$queue, language = lang)
        results$res4 <- kolejkeR::get_raw_data(input$of)
        }
    )
    output$result1 <- renderText(results$res1)
    output$result2 <- renderText(results$res2)
    output$result3 <- renderText(results$res3)
    output$result4 <- renderDT(results$res4)
    output$result5 <- renderText(i18n$t("Summary"))
}

shinyApp(ui = ui, server = server)

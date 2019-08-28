#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(DT)

# Objects in this file are shared across all sessions in the same R process
#source('all_sessions.R', local = TRUE)
options(shiny.sanitize.errors = TRUE)

###################
# Define UI for application that draws a histogram
ui <- tagList(
    includeCSS("styles.css"),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"), 
    navbarPage(
        "Deploy demo", 
        
        ################### TAB amazon   ################### 
        tabPanel(title = "Toxic content (EN)",
                 #setBackgroundImage(src="logo-white@2000x1910.png"),
                 h2("Review predictor demo"),
                 # input field
                 p("Give a review:"),
                 textAreaInput("user_text_toxic", label = NULL, 
                               placeholder = "Please enter some text.",width = "800px"
                 ),
                 
                 # display text output
                 hr(),
                 h4("Inputted description"),
                 textOutput("text_toxic"),
                 tags$head(tags$style("#text1{font-style: italic;
                                  }"
                 )
                 )
                 ,
                 # change style:    
                 #,tags$head(tags$style("#view table {background-color: white; }", media="screen", type="text/css"))
                 hr(),
                 h4("Model output for toxicity and their similarities"),
                 
                 tableOutput("view_table_toxic") %>% withSpinner(color="#0dc5c1")
                 
        )#end of tabPanel
        ,
        # tabPanel(title = "Amazon reviews (EN)",
        #          #setBackgroundImage(src="logo-white@2000x1910.png"),
        #          h2("Review predictor demo"),
        #          # input field
        #          p("Give a review:"),
        #          textAreaInput("user_text_en", label = NULL, 
        #                        placeholder = "Please enter some text.",width = "800px"
        #          ),
        #          
        #          # display text output
        #          hr(),
        #          h4("Inputted description"),
        #          textOutput("text_en"),
        #          tags$head(tags$style("#text1{font-style: italic;
        #                           }"
        #          )
        #          )
        #          ,
        #          # change style:    
        #          #,tags$head(tags$style("#view table {background-color: white; }", media="screen", type="text/css"))
        #          hr(),
        #          h4("Model output for reviews and their similarities"),
        #          
        #          tableOutput("view_table_en") %>% withSpinner(color="#0dc5c1")
        #          
        # )#end of tabPanel
        # ,
        
        tabPanel(title = "Ruimtehol example (NL)",
                 #setBackgroundImage(src="logo-white@2000x1910.png"),
                 h2("Ruimetehol predictor demo"),
                 # input field
                 p("Write some dutch text"),
                 textAreaInput("user_text_nl", label = NULL, 
                               placeholder = "Please enter some text.",width = "800px"
                 ),
                 
                 # display text output
                 hr(),
                 h4("Inputted description"),
                 textOutput("text_nl"),
                 tags$head(tags$style("#text1{font-style: italic;
                                  }"
                 )
                 )
                 ,
                 # change style:    
                 #,tags$head(tags$style("#view table {background-color: white; }", media="screen", type="text/css"))
                 hr(),
                 h4("Model output for topics and their similarities"),
                 
                 tableOutput("view_table_nl") %>% withSpinner(color="#0dc5c1")
                 
        ) #end of tabPanel
        
    ) # end of navbar page
) ##end of ui

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # text output for the first page
    output$text_toxic <- renderText({
        input$user_text_toxic
    })
    
    
    ##toxicity
    
    output$view_table_toxic <- renderTable({
        make_prediction_en(input$user_text_toxic) %>%
            top_n(5)
    },hover=TRUE)
    # text output for the first page
    output$text_toxic <- renderText({
        input$user_text_toxic
    })
    
    
    # ##amazon reviews
    # 
    # output$view_table_en <- renderTable({
    #     make_prediction_en(input$user_text_en) %>%
    #         top_n(5)
    # },hover=TRUE)
    # 
    # 
    # # text output for the first page
    # output$text_nl <- renderText({
    #     input$user_text_nl
    # })
    
    
    ##ruimtehol example
    
    output$view_table_nl <- renderTable({
        make_prediction_nl(input$user_text_nl) %>%
            top_n(5)
    },hover=TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

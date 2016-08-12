library(shiny)
library(stringr)
library(DT)


# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
        # Detect prediction activated
        startPrediction <- reactive({
                input$predict
        })
        
        # Proces sentence
        byWord <- reactive({
                startPrediction()
                isolate({
                #### Preproces input string ####
                # To lower #
                inputSentence <- tolower(input$sentence)
                # Keep only letters
                inputSentence <- str_replace_all(inputSentence, "[^[:alpha:]]", " ")
                # Trim white spaces
                inputSentence <- gsub("\\s+", " ", str_trim(inputSentence))
                # Split by word
                byWord <- unlist(str_split(string = inputSentence, pattern = " "))    
                })
        })
        
        blogsSelected <- reactive({
                startPrediction()
                isolate({
                        
                        ifelse(sum(grepl("1", input$checkGroup)) == 1 || is.null(input$checkGroup), "Option 1 Selected", "Option 1 Not Selected")
                })
        })
        
        newsSelected <- reactive({
                startPrediction()
                isolate({
                        ifelse(sum(grepl("2", input$checkGroup)) == 1 || is.null(input$checkGroup), "Option 2 Selected", "Option 2 Not Selected")
                })
        })
        twitterSelected <- reactive({
                startPrediction()
                isolate({
                        ifelse(sum(grepl("3", input$checkGroup)) == 1 || is.null(input$checkGroup), "Option 3 Selected", "Option 3 Not Selected")
                })
        })
        
        # Number of words
        byWordLength <- reactive({
                byWordLength <- length(byWord())      
        })
        
        
        output$sentence <- renderText({
                startPrediction()
                isolate({
                        input$sentence        
                })
        })
       
        observe({
                startPrediction()
                isolate({
                updateNumericInput(session, "num", value = 22)
                })
        })
        
        output$word1 <- renderText({ 
                print(blogsSelected())
        })
        output$word2 <- renderText({
                print(newsSelected())
        })
        output$word3 <- renderText({
                print(twitterSelected())
        })
        output$value1 <- renderText({
                print(input$slider2)
        })
        output$value2 <- renderText({
                print(input$slider1)
        })
        output$value3 <- renderText({
                print(paste(input$select, " ", input$radio))
        })
        output$value4 <- renderText({
                print(paste(input$num, " ", input$text, " ", input$dates))
        })
        output$ex1 <- DT::renderDataTable(
                DT::datatable(iris, style = 'bootstrap', options = list(pageLength = 25, fixedHeader = TRUE))
        )
})
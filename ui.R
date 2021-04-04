#libray load
library(tidyverse)
library(shiny)
library(stringr)

#Code with algorithm for word prediction
source("prediction.R")

#App front code
ui <- fluidPage(
    
    titlePanel("Capstone Project"),
    p("The app ask for a phrase to the user and return a predicted word based in NPL analysis."),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            
            h3("Enter a group of words in the text item and a predicted word is showed down.")
        ),
        mainPanel(
                         textInput("sentence", h3("Enter please a sentence:"), 
                                   value = "write a sentence"),
                         h3("Predicted Word:"),
                         h5(textOutput("predicted_word"))
        )
    )
)
#server side in the same ui code.
server <- function(input, output) {
    
  output$predicted_word <- renderText({
    prediction(input$sentence)
    })
    
}

shinyApp(ui = ui, server = server)


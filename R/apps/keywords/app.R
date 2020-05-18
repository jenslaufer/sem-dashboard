library(tidyverse)
library(zoo)
library(shiny)
library(semtools)

ui <- fluidPage(titlePanel("Keyword Analysis"),
                
                sidebarLayout(
                    sidebarPanel(
                        fileInput(
                            "file1",
                            "Choose CSV File",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")
                        ),
                        sliderInput(
                            "bins",
                            "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30
                        )
                    ),
                    
                    mainPanel(plotOutput("distPlot"))
                ))

server <- function(input, output) {
    output$distPlot <- renderPlot({
        req(input$file1)
        tryCatch(
            {
                data <- (input$file1 %>% pull(datapath)) %>% semtools::load.keywords()
            },
            error = function(e) {
                print(e)
                stop(safeError(e))
            }
        )
        
        
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        hist(x,
             breaks = bins,
             col = 'darkgray',
             border = 'white')
    })
}

shinyApp(ui = ui, server = server)

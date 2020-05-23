library(tidyverse)
library(zoo)
library(ggrepel)
library(shiny)
library(semtools, verbose = T)
library(DT)
library(logging)
library(glue)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)


JScode <-
    "$(function() {
    setTimeout(function(){
      var vals = [0];
      var powStart = 5;
      var powStop = 2;
      for (i = powStart; i >= powStop; i--) {
        var val = Math.pow(10, -i);
        val = parseFloat(val.toFixed(8));
        vals.push(val);
      }
      $('#pvalue').data('ionRangeSlider').update({'values':vals})
    }, 5)})"

.slider.input <- function(name,
                          title,
                          .min = 0,
                          .max = 1,
                          .value = c(.min, .max)) {
    sliderInput(name,
                title,
                min = .min,
                max = .max,
                value = .value)
}



.update.slider <- function(data, session, field, name = field) {
    max <- data %>% pull(field) %>% max(na.rm = TRUE)
    min <-
        data %>% pull(field) %>% min(na.rm = TRUE)
    
    logdebug(".update.slider: min: {min}, max: {max}" %>% glue())
    
    updateSliderInput(
        session,
        name,
        max = max,
        min = min,
        value = c(min, max)
    )
}

.update.numeric.range.input <-
    function(data, session, field, name = field) {
        logdebug(".update.numeric.range.input...")
        max <- data %>% pull(field) %>% max(na.rm = TRUE)
        min <-
            data %>% pull(field) %>% min(na.rm = TRUE)
        
        logdebug(".update.numeric.range.input: min: {min}, max: {max}" %>% glue())
        
        updateNumericRangeInput(session,
                                name,
                                label = "",
                                value = c(min, max))
    }



ui <- fluidPage(
    
    #theme = shinytheme("cosmo"),
    #chooseSliderSkin("Modern", "DimGray"),
    titlePanel("Keyword Analysis"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput(
                "keywordFiles",
                "Choose CSV File(s)",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
            ),
            
            selectInput(
                "xScale",
                "Scaling X",
                list("Logarithmic" = "log10", "Linear" = "identity"),
                selected = "Logarithmic"
            ),
            selectInput(
                "yScale",
                "Scaling Y",
                list("Logarithmic" = "log10", "Linear" = "identity"),
                selected = "Logarithmic"
            ),
            .slider.input("competition", "Competition", 0, 100),
            .slider.input("bid", "Bid"),
            .slider.input("avg.monthly.searches", "Average Monthy Searches"),
            radioButtons("plotLabels",
                         "Plot Labels",
                         c("Yes" = T,
                           "No" = F), selected = F),
            .slider.input("alpha", "Alpha", .value = 1)
            
        ),
        
        mainPanel(
            plotOutput("distributionPlot"),
            plotOutput("keywordPlot") %>% withSpinner(type = 6),
            dataTableOutput("data")
        )
    )
)

server <- function(input, output, session) {
    initial <- reactive({
        req(input$keywordFiles)
        data <-
            (input$keywordFiles %>% pull(datapath)) %>%
            semtools::load.keywords()
        print(data %>%  arrange(avg.monthly.searches) %>% head(20))
        
        
        .update.slider(data, session, "bid")
        .update.slider(data, session, "avg.monthly.searches")
        
        input$keywordFiles
    })
    
    data <- reactive({
        tryCatch({
            data <-
                (initial() %>% pull(datapath)) %>%
                semtools::load.keywords()
            
            data %>%
                filter(competition >= input$competition[1] &
                           competition <= input$competition[2]) %>%
                filter(
                    avg.monthly.searches >= input$avg.monthly.searches[1] &
                        avg.monthly.searches <= input$avg.monthly.searches[2]
                ) %>%
                filter(bid >= input$bid[1] &
                           bid <= input$bid[2])
            
        },
        error = function(e) {
            logerror(e)
            stop(safeError(e))
        })
    })
    
    output$keywordPlot <- renderPlot({
        semtools::keyword.plot(
            data(),
            .alpha = input$alpha,
            .x.trans = input$xScale,
            .y.trans = input$yScale,
            .labels = input$plotLabels
        )
    })
    
    output$distributionPlot <- renderPlot({
        semtools::distribution.quantitative.plot(data())
    })
    
    output$data <-
        renderDataTable(bind_cols(
            data() %>% select_at(., "keyword"),
            data() %>% select_if(is.numeric)
        ))
}
basicConfig(level = 10)
shinyApp(ui = ui, server = server)

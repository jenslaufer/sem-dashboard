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



.slider.input <- function(data,
                          field,
                          title = field) {
    max <- data %>% pull(field) %>% max(na.rm = TRUE)
    min <-
        data %>% pull(field) %>% min(na.rm = TRUE)
    sliderInput(
        field,
        title,
        min = min,
        max = max,
        value = c(min, max)
    )
}


ui <- fluidPage(
    theme = shinytheme("cosmo"),
    chooseSliderSkin("Modern", "DimGray"),
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
            radioButtons("plotLabels",
                         "Plot Labels",
                         c("Yes" = T,
                           "No" = F), selected = F),
            sliderInput(
                "alpha",
                "Alpha",
                min = 0,
                max = 1,
                value = 1
            ),
            uiOutput("sliders")
            
        ),
        
        mainPanel(
            plotOutput("distributionPlot"),
            plotOutput("keywordPlot") %>% withSpinner(type = 6),
            dataTableOutput("data")
        )
    )
)

server <- function(input, output, session) {
    keyword.files <- reactive({
        req(input$keywordFiles)
        input$keywordFiles
    })
    
    initial.data <- reactive({
        (keyword.files() %>% pull(datapath)) %>%
            semtools::load.keywords()
    })
    
    filtered.data <- reactive({
        tryCatch({
            data <-
                (keyword.files() %>% pull(datapath)) %>%
                semtools::load.keywords()
            
            query <- data %>% select_if(is.numeric) %>%
                colnames() %>%
                map(
                    function(feature)
                        "between({feature}, input${feature}[1], input${feature}[2])" %>% glue()
                ) %>% paste0(collapse = ",")
            
            eval(parse(text = "data %>% filter({query})" %>% glue()))
            
        },
        error = function(e) {
            logerror(e)
            stop(safeError(e))
        })
    })
    
    output$sliders <- renderUI({
        data <- initial.data()
        data %>%
            select_if(is.numeric) %>%
            colnames() %>%
            map( ~ .slider.input(data = data, field = .))
    })
    
    output$keywordPlot <- renderPlot({
        filtered.data() %>% semtools::keyword.plot(
            .alpha = input$alpha,
            .x.trans = input$xScale,
            .y.trans = input$yScale,
            .labels = input$plotLabels
        )
    })
    
    output$distributionPlot <- renderPlot({
        filtered.data() %>% semtools::distribution.quantitative.plot()
    })
    
    output$data <-
        renderDataTable(bind_cols(
            filtered.data() %>% select_at(., "keyword"),
            filtered.data() %>% select_if(is.numeric)
        ))
}
basicConfig(level = 10)
shinyApp(ui = ui, server = server)

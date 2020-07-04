library(tidyverse)
library(zoo)
library(ggrepel)
library(shiny)
library(semtools)
library(DT)
library(logging)
library(glue)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(ggfortify)
library(broom)



.slider.input <- function(data,
                          field,
                          title = field) {
    max <- data %>%
        pull(field) %>%
        max(na.rm = TRUE)
    min <-
        data %>%
        pull(field) %>%
        min(na.rm = TRUE)
    
    
    list(
        renderPlot({
            data %>%
                semtools::distribution.quantitative.plot(field)
        }, width = 250, height = 150),
        numericRangeInput(
            inputId = field,
            label = title,
            value = c(min, max)
        )
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
                list(
                    "Logarithmic" = "log10",
                    "Linear" = "identity",
                    "sqrt" = "sqrt"
                ),
                selected = "identity"
            ),
            selectInput(
                "yScale",
                "Scaling Y",
                list(
                    "Logarithmic" = "log10",
                    "Linear" = "identity",
                    "sqrt" = "sqrt"
                ),
                selected = "identity"
            ),
            selectInput(
                "sizeScale",
                "Scaling Size",
                list(
                    "Logarithmic" = "log10",
                    "Linear" = "identity",
                    "sqrt" = "sqrt"
                ),
                selected = "identity"
            ),
            selectInput(
                "colorScale",
                "Scaling Color",
                list(
                    "Logarithmic" = "log10",
                    "Linear" = "identity",
                    "sqrt" = "sqrt"
                ),
                selected = "identity"
            ),
            sliderInput(
                "alpha",
                "Alpha",
                min = 0,
                max = 1,
                value = 1
            ),
            uiOutput("axisControl"),
            uiOutput("sliders")
        ),
        mainPanel(
            plotOutput("keywordFilterPlot", brush = "keywordPlotBrush") %>% withSpinner(type = 6),
            plotOutput("keywordPlot", click = "keywordPlotClick"),
            dataTableOutput("data")
        )
    )
)

server <- function(input, output, session) {
    data <- reactiveValues(keywords = tibble())
    
    
    
    initial_data <- eventReactive(input$keywordFiles, {
        data$keywords <- (input$keywordFiles %>% pull(datapath)) %>%
            semtools::load.semrush.keywords() %>%
            select(-Trend,-serp_features) %>%
            mutate(id = row_number())
        
        data$keywords <- data$keywords %>%
            mutate(included = if ("included" %in% colnames(.))
                included
                else
                    F)
        data$keywords
    })
    
    brushed_data <- reactive({
        brushedPoints(
            data$keywords,
            brush = input$keywordPlotBrush,
            xvar = input$xFeature,
            yvar = input$yFeature
        )
    })
    
    filtered_data <- reactive({
        tryCatch({
            initial_data()
            
            data <- data$keywords
            
            
            query <- data %>%
                select_if(is.numeric) %>%
                colnames() %>%
                map(function(feature) {
                    "between({feature}, input${feature}[1], input${feature}[2])" %>% glue()
                }) %>%
                paste0(collapse = ",")
            eval(parse(text = "data %>% filter({query})" %>% glue()))
            
        },
        error = function(e) {
            logerror(e)
            stop(safeError(e))
        })
    })
    
    output$exportData <- downloadHandler(
        filename = function() {
            paste("labeled-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            data$keywords %>% write_csv(file)
        }
    )
    
    observeEvent(input$data_rows_selected, {
        filtered_data <- filtered_data()
        data <- data$keywords
        
        selected_keyword <- filtered_data %>%
            slice(input$data_rows_selected) %>%
            pull(keyword)
        
        data$keywords <- data %>%
            mutate(included = if_else(keyword == selected_keyword,!included,
                                      included))
        
    })
    
    observeEvent(input$ok, {
        
    })
    
    observeEvent(input$keywordPlotClick, {
        dataPoints <- data$keywords %>%
            nearPoints(input$keywordPlotClick, addDist = TRUE)
        
        if (dataPoints %>% nrow() > 0) {
            showModal(
                modalDialog(
                    title = "Somewhat important message",
                    "This is a somewhat important message.",
                    easyClose = TRUE,
                    footer = tagList(actionButton("ok", "OK"))
                )
            )
        }
        
    })
    
    output$axisControl <- renderUI({
        data <- data$keywords
        cols <- data %>%
            select(-keyword) %>%
            colnames()
        cols <-  cols %>%
            as.list() %>%
            setNames(cols)
        
        numeric_cols <-
            data %>%
            select_if(is.numeric) %>%
            colnames()
        
        
        list(
            selectInput("xFeature",
                        "Feature x Encoding",
                        cols,
                        selected = numeric_cols[1])
            ,
            selectInput("yFeature",
                        "Feature y Encoding",
                        cols,
                        selected = numeric_cols[2])
            ,
            selectInput(
                "colorFeature",
                "Feature color Encoding",
                cols,
                selected = numeric_cols[3]
            )
            ,
            selectInput(
                "sizeFeature",
                "Feature size Encoding",
                cols,
                selected = numeric_cols[4]
            )
            ,
            downloadButton("exportData", "Export...")
        )
    })
    
    output$sliders <- renderUI({
        data <- initial_data()
        data %>%
            select_if(is.numeric) %>%
            colnames() %>%
            map(~ .slider.input(data = data, field = .))
    })
    
    output$keywordPlot <- renderPlot({
        data <-  brushed_data()
        plotLabels <- F
        if (data %>% nrow() <= 75) {
            plotLabels <- T
        }
        
        data %>%
            semtools::keyword.plot(
                x.feature.name = input$xFeature,
                y.feature.name = input$yFeature,
                color.feature.name = input$colorFeature,
                size.feature.name = input$sizeFeature,
                .alpha = input$alpha,
                .x.trans = input$xScale,
                .y.trans = input$yScale,
                .size.trans = input$sizeScale,
                .color.trans = input$colorScale,
                .labels = plotLabels
            )
    })
    
    
    output$keywordFilterPlot <- renderPlot({
        data <-  filtered_data()
        plotLabels <- F
        if (data %>% nrow() <= 75) {
            plotLabels <- T
        }
        
        data %>%
            semtools::keyword.plot(
                x.feature.name = input$xFeature,
                y.feature.name = input$yFeature,
                color.feature.name = input$colorFeature,
                size.feature.name = input$sizeFeature,
                .alpha = input$alpha,
                .x.trans = input$xScale,
                .y.trans = input$yScale,
                .size.trans = input$sizeScale,
                .color.trans = input$colorScale,
                .labels = plotLabels
            )
    })
    
    
    output$data <-
        renderDataTable({
            bind_cols(filtered_data())
        }, selection = "single")
    
}
basicConfig(level = 10)
shinyApp(ui = ui, server = server)
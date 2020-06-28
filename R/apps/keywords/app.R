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
            plotOutput("distributionPlot"),
            plotOutput("keywordPlot", brush = "distributionPlotBrush") %>% withSpinner(type = 6),
            dataTableOutput("data")
        )
    )
)

server <- function(input, output, session) {
    data <- reactiveValues(keywords = tibble())
    
    
    
    initial_data <- eventReactive(input$keywordFiles, {
        data$keywords <- (input$keywordFiles %>% pull(datapath)) %>%
            semtools::load.semrush.keywords() %>%
            mutate(id = row_number())  %>%
            as_tibble() %>%
            mutate(cpc_chance = 1 / cpc) %>%
            filter(!is.na(cpc_chance) & !is.infinite(cpc_chance))
        
        
        data$keywords <- data$keywords %>%
            mutate(included = if ("included" %in% colnames(.))
                included
                else
                    F)
        data$keywords
    })
    
    brushed_data <- reactive({
        initial_data()
        brushedPoints(
            data$keywords,
            brush = input$distributionPlotBrush,
            xvar = input$xFeature,
            yvar = input$yFeature
        )
    })
    
    filtered_data <- reactive({
        tryCatch({
            initial_data()
            data <- data$keywords
            
            result <- brushed_data()
            
            if (result
                %>% nrow() == 0) {
                query <- data %>%
                    select_if(is.numeric) %>%
                    colnames() %>%
                    map(function(feature) {
                        "between({feature}, input${feature}[1], input${feature}[2])" %>% glue()
                    }) %>%
                    paste0(collapse = ",")
                
                logging::logdebug("data %>% filter({query})" %>% glue())
                
                result <-
                    eval(parse(text = "data %>% filter({query})" %>% glue()))
            }
            
            result
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
        
        selected_keyword %>%  print()
        
        data$keywords <- data %>%
            mutate(included = if_else(keyword == selected_keyword,!included,
                                      included))
        
        print(data$keywords %>% select(keyword, included) %>%
                  arrange(-included))
        
    })
    
    output$axisControl <- renderUI({
        data <- filtered_data()
        cols <- data %>%
            select(-keyword) %>%
            colnames()
        cols <-  cols %>%
            as.list() %>%
            setNames(cols)
        
        print(cols)
        
        
        list(
            selectInput("xFeature",
                        "Feature x Encoding",
                        cols,
                        selected = "avg.monthly.searches")
            ,
            selectInput("yFeature",
                        "Feature y Encoding",
                        cols,
                        selected = "competition")
            ,
            selectInput(
                "colorFeature",
                "Feature color Encoding",
                cols,
                selected = "cpc"
            )
            ,
            selectInput("sizeFeature",
                        "Feature size Encoding",
                        cols,
                        selected = "cpc_chance")
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
        data <-  filtered_data()
        plotLabels <- F
        if (data %>% nrow() < 100) {
            plotLabels <- T
        }
        
        plot <- data %>%
            semtools::keyword.plot(
                x.feature.name = input$xFeature,
                y.feature.name = input$yFeature,
                color.feature.name = input$colorFeature,
                size.feature.name = input$sizeFeature,
                .alpha = input$alpha,
                .x.trans = input$xScale,
                .y.trans = input$yScale,
                .labels = plotLabels
            )
        if (data %>%
            select(input$colorFeature) %>%
            map_chr(class) == "numeric") {
            plot <- plot +
                scale_colour_gradientn(colours = terrain.colors(10))
        }
        plot
    })
    
    
    output$distributionPlot <- renderPlot({
        filtered_data() %>%
            semtools::distribution.quantitative.plots()
    })
    
    
    output$data <-
        renderDataTable({
            bind_cols(filtered_data())
        }, selection = "single")
    
}
basicConfig(level = 10)
shinyApp(ui = ui, server = server)
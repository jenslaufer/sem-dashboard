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



.slider_input <- function(data,
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

.slider_input_update <-
    function(data, field, title = field, session) {
        max <- data %>%
            pull(field) %>%
            max(na.rm = TRUE)
        min <-
            data %>%
            pull(field) %>%
            min(na.rm = TRUE)
        
        updateNumericRangeInput(
            inputId = field,
            label = title,
            session = session,
            value = c(min, max)
        )
    }

.get_data_point <- function(data, field) {
    dataPoints <- data %>%
        nearPoints(field, addDist = F)
    if (dataPoints %>% nrow() > 0) {
        dataPoints %>% head(1)
    } else{
        NULL
    }
}
.get_modal <- function(clicked_data_point) {
    details <-
        clicked_data_point %>% colnames() %>%
        map(~ paste(.x, ": ", clicked_data_point %>% pull(.x), "<br>")) %>%
        as.character()
    
    button_label <- "Include"
    if (clicked_data_point %>% pull(included) == T) {
        button_label <- "Exclude"
    }
    
    modalDialog(
        title = "",
        HTML(details),
        easyClose = TRUE,
        footer = tagList(actionButton("ok", button_label))
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
            plotOutput("keywordFilterPlot", brush = "keywordPlotBrush", click = "keywordFilterPlotClick") %>% withSpinner(type = 6),
            plotOutput("keywordPlot", click = "keywordPlotClick"),
            dataTableOutput("data")
        )
    )
)

server <- function(input, output, session) {
    data <- reactiveValues(rows = tibble())
    
    initial_data <- eventReactive(input$keywordFiles, {
        data$rows <- (input$keywordFiles %>% pull(datapath)) %>%
            semtools::load.semrush.keywords() %>%
            mutate(id = row_number())
        
        data$rows <- data$rows %>%
            mutate(included = if ("included" %in% colnames(.))
                included
                else
                    F)
        data$rows
    })
    
    brushed_data <- reactive({
        initial_data()
        brushedPoints(
            data$rows,
            brush = input$keywordPlotBrush,
            xvar = input$xFeature,
            yvar = input$yFeature
        )
    })
    
    filtered_data <- reactive({
        tryCatch({
            initial_data()
            
            data <- data$rows
            
            
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
            data$rows %>% write_csv(file)
        }
    )
    
    observeEvent(input$data_rows_selected, {
        filtered_data <- filtered_data()
        
        selected <- filtered_data %>%
            slice(input$data_rows_selected)
        showModal(.get_modal(selected))
    })
    
    observeEvent(input$ok, {
        if (!is.null(input$keywordPlotClick)) {
            clicked_id <-
                .get_data_point(data$rows, input$keywordPlotClick) %>% pull(id)
        } else if (!is.null(input$keywordFilterPlotClick)) {
            clicked_id <-
                .get_data_point(data$rows, input$keywordFilterPlotClick) %>% pull(id)
        } else if (!is.null(input$data_rows_selected)) {
            filtered_data <- data$rows
            clicked_id <- filtered_data %>%
                slice(input$data_rows_selected) %>% pull(id)
        }
        
        
        data$rows <-
            data$rows %>%
            mutate(included = if_else(id == clicked_id, !included, included))
        removeModal()
    })
    
    observeEvent(input$keywordPlotClick, {
        clicked_data_point <-
            .get_data_point(data$rows, input$keywordPlotClick)
        
        if (!is.null(clicked_data_point)) {
            showModal(.get_modal(clicked_data_point))
        }
        
    })
    
    observeEvent(input$keywordFilterPlotClick, {
        clicked_data_point <-
            .get_data_point(data$rows, input$keywordFilterPlotClick)
        
        if (!is.null(clicked_data_point)) {
            showModal(.get_modal(clicked_data_point))
        }
        
    })
    
    observeEvent(input$filterClear, {
        data <- data$rows
        data %>%
            select_if(is.numeric) %>%
            select(-id) %>%
            colnames()  %>%
            map(~ .slider_input_update(
                data = data,
                field = .,
                session = session
            ))
        
    })
    
    output$axisControl <- renderUI({
        data <- initial_data()
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
            ),
            downloadButton("exportData", "Export..."),
            actionButton("filterClear", "Clear filters")
        )
    })
    
    output$sliders <- renderUI({
        data <- initial_data()
        data %>%
            select_if(is.numeric) %>%
            colnames() %>%
            map(~ .slider_input(data = data, field = .))
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
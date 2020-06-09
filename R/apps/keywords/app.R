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
            radioButtons("plotLabels",
                         "Plot Labels",
                         c("Yes" = T,
                           "No" = F),
                         selected = F),
            sliderInput(
                "alpha",
                "Alpha",
                min = 0,
                max = 1,
                value = 1
            ),
            uiOutput("sliders")
        ),
        mainPanel(tabsetPanel(
            tabPanel(
                "Analysis",
                plotOutput("distributionPlot"),
                plotOutput("keywordPlot") %>% withSpinner(type = 6),
                textOutput(outputId = "myText"),
                dataTableOutput("data")
            ),
            tabPanel("Tagging", dataTableOutput("taggingData"))
        ))
    )
)

server <- function(input, output, session) {
    data <- reactiveValues(keywords = tibble())
    
    initial_data <- eventReactive(input$keywordFiles, {
        data$keywords <- (input$keywordFiles %>% pull(datapath)) %>%
            semtools::load.keywords() %>%
            mutate(id = row_number(), included = F) %>%
            mutate(id = as.character(id)) %>%
            as_tibble()
        data$keywords
    })
    
    
    shinyInput <- function(FUN, name, id, ...) {
        as.character(FUN(paste0(name, id), ...))
    }
    
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
    
    observeEvent(input$select_button, {
        event <- input$select_button %>% str_split("_")
        print(event)
        
        command <- event %>% map(1)
        selectedId <- event %>% map(2) %>% as.numeric()
        
        data$keywords <-
            data$keywords %>% mutate(
                included = case_when(
                    id == data$keywords %>%
                        filter(id == selectedId) %>% pull(id) &
                        command == "Include" ~ T,
                    id == data$keywords %>%
                        filter(id == selectedId) %>% pull(id) &
                        command == "Exclude" ~ F,
                    T ~ included
                )
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
        filtered_data() %>%
            mutate(bid.chance = 1 / bid) %>%
            semtools::keyword.plot(
                .alpha = input$alpha,
                .x.trans = input$xScale,
                .y.trans = input$yScale,
                .labels = input$plotLabels
            ) +
            scale_colour_gradientn(colours = terrain.colors(10))
    })
    
    
    
    output$distributionPlot <- renderPlot({
        filtered_data() %>%
            semtools::distribution.quantitative.plots()
    })
    
    
    output$data <-
        renderDataTable({
            bind_cols(filtered_data())
        })
    
    output$taggingData <-
        renderDataTable({
            bind_cols(filtered_data() %>%
                          mutate(Actions = map2(
                              id,
                              included,
                              ~ shinyInput(
                                  actionButton,
                                  if_else(.y, "Exclude_", "Include_"),
                                  .x,
                                  label = if_else(.y, "Exclude", "Include"),
                                  onclick = paste0('Shiny.setInputValue( \"select_button\" , this.id)')
                              )
                          )))
        }, escape = FALSE)
}
basicConfig(level = 10)
shinyApp(ui = ui, server = server)
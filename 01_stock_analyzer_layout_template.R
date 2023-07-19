library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)
library(lubridate)
library(rvest)
library(glue)
library(zoo)

source(file = "./helper_functions.R")

# UI
ui <- fluidPage(title = "Stock Analyzer",
                # Header
                div(
                  h1("Stock Analyzer"),
                ),
                # App
                div(fluidRow(
                  column(
                    div(h4("Click on dropdown button")),
                    width = 4,
                    size = 10,
                    multiple = F, 
                    actionsBox = FALSE,
                    div(
                    dropdown(
                    wellPanel(
                      strong("Index List"),
                      selectInput(
                        inputId = "indices",
                        label = h4("Select index"),
                        choices = c("SP500", "DOW", "NASDAQ","DAX"),
                        multiple = FALSE
                      ),
                      strong("Stock List"),
                      
                      uiOutput("indices"),
                      
                      dateRangeInput(inputId = "date_range_picker", 
                                     label   = h4("Date Range"), 
                                     start   = "2015-01-01", 
                                     end     = today(), 
                                     startview = "year"),
                      
                      hr(),
                      h4("Moving Averages"),
                      sliderInput(inputId = "short_mavg_slider", 
                                  label   = "Short Moving Average", 
                                  min     = 5,
                                  max     = 40, 
                                  value   = 20, 
                                  step    = 1, 
                                  round   = TRUE),
                      
                      sliderInput(inputId = "long_mavg_slider", 
                                  label   = "Long Moving Average", 
                                  min     = 50,
                                  max     = 120, 
                                  value   = 50, 
                                  step    = 1, 
                                  round   = TRUE), 
                      
                      actionButton(inputId = "analyze_button", 
                                   label   = "Analyze",
                                   icon    = icon("refresh"))
                      
                    ),
                    circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                    tooltip = tooltipOptions(title = "Click to see inputs !")
                    )
                  )), 
                  column(
                    width = 8,
                    div(
                      div(h4(textOutput(outputId = "plot_header"))),
                      div(plotlyOutput("output_chart")),
                      div(h4("Analyst Commentary")),
                      div(p(textOutput(outputId = "analyst_commentary"))
                      
                    )
                  )
                ))
))


server <- function(input, output, session) {
  output$indices <- renderUI({
    stock_list_tbl <- get_stock_list(stock_index = input$indices)
    print(colnames(stock_list_tbl))
    selectInput(
      inputId = "stock_selection_input",
      label = h4("Select stock"),
      choices = stock_list_tbl$label,
    )
  })
  
  # Event obsorver so everything is reactive
  observeEvent(eventExpr = input$analyze_button, handlerExpr = {
    ui_update()
  })
  
  observeEvent(eventExpr = input$short_mavg_slider, handlerExpr = {
    ui_update()
  })
  
  observeEvent(eventExpr = input$long_mavg_slider, handlerExpr = {
    ui_update()
  })
  
  observeEvent(eventExpr = input$date_range_picker, handlerExpr = {
    ui_update()
  }) 
  
  # Make UI reactive by updating the components
  ui_update <- function() {
    stock_symbol <- eventReactive(input$analyze_button, {
      input$stock_selection_input
    })
    
    symbol <- get_symbol_from_user_input(stock_symbol())[1]
    
    output$plot_header <- renderText(stock_symbol())
    
    # Input Data
    start_date = input$date_range_picker[1] 
    end_date = input$date_range_picker[2] 
    sma <- input$short_mavg_slider
    lma <- input$long_mavg_slider
    
    # Call helper function
    stock_data_table = get_stock_data(
      stock_symbol = symbol, 
      from = start_date, 
      to   = end_date,
      mavg_short = sma,
      mavg_long  = lma)
    
    output$output_chart <- renderPlotly(stock_data_table %>%  plot_stock_data()) 
    
    output$analyst_commentary <- renderText(generate_commentary(data = stock_data_table, user_input = stock_symbol()))
  }
}
# Run app
shinyApp(ui = ui, server = server)
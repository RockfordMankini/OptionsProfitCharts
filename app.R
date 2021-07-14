#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("options.R")
library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Options Profit/Loss Charts"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Strike Price and Option Price input is necessary. If you're longing/shorting a stock, strike price will not be used, 
                     and the Option Price will be replaced by the Stock Price."),
            numericInput("strike", label = h3("Strike Price"), value = 10),
            numericInput("price", label = h3("Option/Stock Price"), value = 1),
            radioButtons("type", label = h3("Option Type"),
                         choices = list("Call" = "Call", "Put" = "Put", "Stock" = "Stock"), 
                         selected = "Call"),
            radioButtons("write", label = h3("Long or short (Purchase or write)?"),
                         choices = list("Long" = FALSE, "Short" = TRUE), 
                         selected = FALSE),
            textInput("name", "Option Name", "Option #1"),
            actionButton("addOption", label = "Add Option"),
            br(),
            br(),
            actionButton("clearOptions", label = "Clear Options")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(      # Copy the line below to make a slider range 
            sliderInput("range", label = h3("Stock Price Range"), min = 0, 
                        max = 500, value = c(0, 100)),
            plotlyOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    options_list <- reactiveVal(list())
    
    observeEvent(input$addOption, {
        req(input$price, input$strike)
        
        options <- length(options_list())
        new_options_list <- options_list()
        new_options_list[[options + 1]] <- optionsProfit(input$strike, input$type, input$price, input$write)
        names(new_options_list)[options + 1] <- input$name
        options_list(new_options_list)
        
    })
    
    observeEvent(input$clearOptions, {
        options_list(list())
    })
    
    output$plot <- renderPlotly({
        plot()
    })
    
    plot <- reactive({
        
        req(length(options_list()) > 0)

        g <- plot_ly()
        
        x <- list(
            title = "Stock Price"
        )
        y <- list(
            title = "Profit"
        )
        
        g <- g %>% layout(xaxis = x, yaxis = y)
        
        lower_bound <- (input$range[1] / .01) + 1
        upper_bound <- (input$range[2] / .01) + 1
        
        if(length(options_list()) > 0) {
            
            max_profit <- 0
            max_loss <- 0
            
            profit_total <- 0
            
            for(i in 1:length(options_list())) {
                option <- options_list()[[i]]
                
                option <- option[lower_bound:upper_bound,]
                
                stock <- option$stock
                profit <- option$profit
                profit_total <- profit_total + profit
                
                if(max(profit) > max_profit) {
                    max_profit <- max(profit)
                }
                
                if(min(profit) < max_loss) {
                    max_loss <- min(profit)
                }
                
                name <- names(options_list())[i]
                
                    
                g <- g %>% add_trace(x = stock, y = profit,  type='scatter', mode = 'lines', name=name)

                
            }
        }
        
        if(length(options_list()) > 1) {
            g <- g %>% add_trace(x = stock, y = profit_total,  type='scatter', mode = 'lines', name="Total Profit")
        }
        
        g %>% layout(xaxis = list(range = c(input$range[1], input$range[2])), yaxis = list(range = c(max_loss, max_profit)))

        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

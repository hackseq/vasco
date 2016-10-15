#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(readr)
library(dplyr)
library(Matrix)
library(plotly)
library(magrittr)


source('helpers.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$tSNEPlot <- renderPlotly({
        # size of the bins depend on the input 'bins'
        plot_ly(tsne, x = ~tSNE_1, y = ~tSNE_2, text = ~barcode, color = ~id, key = ~barcode) %>%
        layout(dragmode = "select")
        
    })
    selected_data <- reactive({event_data("plotly_selected")})
    
    output$brush <- renderPrint({ selected_data()
                                     })
 
    
})


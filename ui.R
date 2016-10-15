#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  verbatimTextOutput("brush"),
  # Application title
  titlePanel("Single cell vis"),
  tabsetPanel( id = "main_panel",
    tabPanel('tSNE',
             fluidRow(column(4,wellPanel(textInput('geneSelect', "Selected Genes", placeholder = 'ENO2, CD8'))),
                      column(8,plotlyOutput('tSNEPlot')),
                      column(4, actionButton(inputId = "plot_selected", label = "plot selected"))
                      
                      )),
    tabPanel('Querry genes'),
    tabPanel(''),
    tabPanel( 'Explore',
                            inputId= 'Explore',
                           fluidRow(column(8,plotlyOutput('newPlot'))
    )
  ) 
))
)

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Single cell vis"),
  tabsetPanel(
    tabPanel('tSNE',
             fluidRow(column(4,wellPanel(textInput('geneSelect', "Selected Genes", placeholder = 'ENO2, CD8'))),
                      column(8,plotlyOutput('tSNEPlot'))
                      ),
    tabPanel('Querry genes'),
    tabPanel('')
    )
  )
))

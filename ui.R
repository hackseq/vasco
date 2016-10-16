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
  # debug output change it at will
  verbatimTextOutput("debug"),
  plotlyOutput('debugPlot'),
  # Application title
  titlePanel("Single cell vis"),
  tabsetPanel( id = "main_panel",
               # main panel for tSNE plot and group selection
               tabPanel('tSNE',
                        # selected button!!
                        fluidRow(
                                 # main window plots
                                 column(8,
                                        plotlyOutput('tSNEPlot'),
                                        plotlyOutput('countPerCluster'))
                        )),
               # panel for displaying individual gene expression data
               tabPanel('geneExpr', 
                        column(4, wellPanel(selectizeInput('input_genes', 'Select genes',
                                                           choices = list_of_genesymbols,
                                                           options = list(maxItems = 4),
                                                           selected = c('CD8A_ENSG00000153563'),
                                                           multiple = TRUE))),
                        column(8, plotlyOutput('geneExprPlot'))
               ),
               # exploration of selection
               tabPanel( 'Compare',
                         inputId= 'Explore',
                         column(8,plotlyOutput('tSNE_select')),
                         column(4,wellPanel( actionButton(inputId = "plot_selected", label = "plot selected")))
                         )
               ) 
  ))


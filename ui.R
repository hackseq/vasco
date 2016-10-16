#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyjs(),
  # debug output change it at will
  verbatimTextOutput("debug"),
  plotlyOutput('difHeatmap'),
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
                         div(id= "div_select_one", column(8,plotlyOutput('tSNE_select_one'))),
                         div(id = "div_select_two", column(8,plotlyOutput('tSNE_select_two'))),
                         column(4,wellPanel( p(id = "select_text", "Please select first population"),
                                             actionButton(inputId = "pop_one_selected", label = "group one"),
                                             actionButton(inputId = "pop_two_selected", label = "group two"))
                                            
                                )
                         )
               ) 
  ))


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
shinyUI(
  fluidPage(
  useShinyjs(),
  extendShinyjs(text = "shinyjs.refresh = function() { redir_Str = window.location.href.split('?')[0] + '?compare'; window.location.href = redir_Str ; }"),
  # debug output change it at will
  verbatimTextOutput("debug"),
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
                        column(4, wellPanel(selectizeInput('input_genes', h1('Select genes'),
                                                           choices = list_of_genesymbols,
                                                           options = list(maxItems = geneExpr_maxItems),
                                                           selected = c('CD8A_ENSG00000153563'),
                                                           multiple = TRUE),
                                            checkboxGroupInput("checkVisualization",
                                                          label = h3("Visualizations"),
                                                          choices = list("tSNE gene expression heatmap" = 1,
                                                          "Histograms" = 2),
                                                           selected = 1),
                                            numericInput("Midpoint", label = h3("Midpoint"), value = 1)
                                          ),
                               actionButton("exprGeneButton", "Plot expression data")
                               ),
                        column(8, uiOutput("geneExprPlot"))
               ),
               # exploration of selection
               tabPanel( 'Compare',
                         id= 'Compare',
                         column(4,wellPanel( h4(id = "select_text", "Please select first population"),
                                             actionButton(inputId = "pop_one_selected", label = "Save group one"),
                                             actionButton(inputId = "pop_two_selected", label = "Save group two"),
                                             br(),
                                             actionButton(inputId = "reload", label = "Select new groups"),
                                             downloadButton(outputId = 'downloadDifGenes', label = 'Download'))),
                         column(8,
                         div(id= "div_select_one", plotlyOutput('tSNE_select_one')),
                         div(id = "div_select_two", plotlyOutput('tSNE_select_two')),
                         div(id= 'comparisonOutput', dataTableOutput('difGeneTable'), plotlyOutput('tSNE_summary'))
                         ))
               )
  )
)


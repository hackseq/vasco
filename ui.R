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
library(colourpicker)
library(shinythemes)


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(theme = shinytheme('lumen'),
  useShinyjs(),
  extendShinyjs(text = "shinyjs.refresh = function() { redir_Str = window.location.href.split('?')[0] + '?compare'; window.location.href = redir_Str ; }"),
  # Application title
  titlePanel("Single cell vis"),
  tabsetPanel( id = "main_panel",
               # main panel for tSNE plot and group selection
               tabPanel('tSNE',
                        # selected button!!
                        fluidRow(
                                 # main window plots
                                 column(8,
                                        plotlyOutput('tSNEPlot',height = '600px'),
                                        plotlyOutput('countPerCluster'))
                        )),
               # panel for displaying individual gene expression data
               tabPanel('geneExpr',
                        column(4, wellPanel(selectizeInput('input_genes', h1('Select genes'),
                                                           choices = list_of_genesymbols,
                                                           options = list(maxItems = geneExpr_maxItems),
                                                           selected = c('CD8A_ENSG00000153563'),
                                                           multiple = TRUE),
                                            radioButtons("checkVisualization",
                                                          label = h3("Visualizations"),
                                                          choices = list("tSNE gene expression heatmap" = 1,
                                                          "Histograms" = 2),
                                                           selected = 1),
                                            conditionalPanel(
                                              condition = "input.checkVisualization == 1 || input.checkVisualization == 2",
                                              actionButton("exprGeneButton", "Plot expression data")
                                            ),
                                            conditionalPanel(
                                              condition = "input.checkVisualization == 1",
                                              sliderInput("MinMax", label= h5("Range of expression:"), min = 0, max = 1, value = c(0,1), step= 0.02),
                                              sliderInput("Midpoint", label= h5("Midpoint:"), min = 0, max = 1, value = 0.5, step= 0.02),
                                              colourInput("colmax", "Select maximum colour", value = geneExpr_colorMax),
                                              colourInput("colmid", "Select midpoint colour", value = geneExpr_colorMid),
                                              colourInput("colmin", "Select minimum colour", value = geneExpr_colorMin)
                                            )
                                          )
                               ),
                        conditionalPanel(
                          condition = "input.checkVisualization == 1",
                          column(8, uiOutput("geneExprPlot"))
                        ),
                        conditionalPanel(
                          condition = "input.checkVisualization == 2",
                          column(8, plotlyOutput("geneExprGeneCluster"))
                        )
               ),
               # exploration of selection
               tabPanel( 'Compare',
                         id= 'Compare',
                         column(2,wellPanel( h4(id = "select_text", "Please select first population"),
                                             actionButton(inputId = "pop_one_selected", label = "Save group one"),
                                             actionButton(inputId = "pop_two_selected", label = "Save group two"),
                                             br(),
                                             div( id = 'definedInputSelection',
                                                  checkboxInput(inputId = 'selectDefinedGroup',
                                                                value = F,
                                                                label = 'Select defined groups?'),
                                                  checkboxGroupInput(inputId = 'whichGroups',
                                                                     label = 'Defined Groups',
                                                                     choices = unique(tsne$id))
                                             ),
                                             actionButton(inputId = "reload", label = "Select new groups"),
                                             downloadButton(outputId = 'downloadDifGenes', label = 'Download'))),
                         column(10,
                         div(id= "div_select_one", plotlyOutput('tSNE_select_one',height = '600px')),
                         div(id = "div_select_two", plotlyOutput('tSNE_select_two',height = '600px')),
                         dataTableOutput('difGeneTable'),
                         div(id= 'comparisonOutput',
                             tabsetPanel(tabPanel('histPlot',
                                                  plotlyOutput('histPlot', height = '500px')),
                                         tabPanel('tSNE plot',
                                                  plotlyOutput('tSNE_summary'), plotlyOutput('cell_type_summary')))
                             )
                         ))
               )
  )
)


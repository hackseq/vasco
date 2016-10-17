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

  tags$head(
    tags$style(HTML("
                  p, li {
                    /**font-family: 'Lora', 'Times New Roman', serif;**/
                    font-size: 20px;
                    color: #404040;
                    line-height: 1.2;
                  }
                  li {
                    font-size: 16px;
                  }
                  body {
                    padding: 0 2.5em 0 2.5em;
                  }
                    li p {
                    margin: 0;
                    padding: 0.1em;
                  }
                    p + ul {
                    margin-top: -10px;
                  }
                    "))
    ),

  extendShinyjs(text = "shinyjs.refresh = function() { redir_Str = window.location.href.split('?')[0] + '?compare'; window.location.href = redir_Str ; }"),
  # Application title
  titlePanel(h1("VASCO", id="title")),
  #p("Visualization App for Single Cells Overview"),

  tabsetPanel( id = "main_panel",
               # main panel for tSNE plot and group selection
               tabPanel('Summary',
                        id = "tSNE",
                        # selected button!!
                        fluidRow(
                                br(),
                                # welcome text
                                column(8, offset = 1,
                                       tags$div(
                                         tags$p("This application facilitates real-time data visualization and exploration of single cell RNA-seq data.
                                                It takes the gene-barcode matrix and the clustering result as input."),
                                         tags$p("Users can:")
                                       ),

                                  tags$ul(
                                  tags$li("visualize the cells using t-distributed stochastic neighbour embedding (t-SNE) plots"),
                                  tags$li("explore the expression pattern of specific genes"),
                                  tags$li("investigate the identity of cell clusters by examining genes that are specific to a cluster")
                                )),

                                # main window plots
                                 column(8,
                                        plotlyOutput('tSNEPlot',height = '600px'),
                                        # we may not want the barchart displayed; see issue#30
                                        # plotlyOutput('countPerCluster'),
                                        br(),
                                        offset=2)
                        )),
               # panel for displaying individual gene expression data
               tabPanel('Visualize genes',
                        id = "geneEpxr",
                        column(4, wellPanel(selectizeInput('input_genes', h1('Select genes'),
                                                           choices = list_of_genesymbols,
                                                           options = list(maxItems = geneExpr_maxItems),
                                                           selected = c('CD8A_ENSG00000153563'),
                                                           multiple = TRUE),
                                            actionButton("exprGeneButton", "Visualize"),
                                            div(id = 'tsneHeatmapOptions',
                                              sliderInput("MinMax", label= h5("Adjust heatmap color scale:"), min = 0, max = 1, value = c(0,1), step= 0.02),
                                              sliderInput("Midpoint", label= h5("Adjust midpoint of color scale:"), min = 0, max = 1, value = 0.5, step= 0.02),
                                              colourInput("colmax", "Select color of scale maximum", value = geneExpr_colorMax),
                                              colourInput("colmid", "Select color of scale midpoint", value = geneExpr_colorMid),
                                              colourInput("colmin", "Select color of scale minimum", value = geneExpr_colorMin)
                                            )
                                          )
                               ),
                        column(8,
                               tabsetPanel(id ='exprVis',
                                           tabPanel('tSNE heatmap', value = 'tSNE',
                                                    uiOutput("geneExprPlot")),
                                           tabPanel('Box plots', value = 'box',
                                                    plotlyOutput("geneExprGeneCluster"))))
               ),
               # exploration of selection
               tabPanel( 'Explore clusters',
                         id= 'Compare',
                         column(3,
                                wellPanel(tabsetPanel(id = 'compareOrSet',
                                                      tabPanel('Compare',
                                                               h4(id = "select_text", "Please select first group"),
                                                               actionButton(inputId = "pop_one_selected", label = "Save group 1"),
                                                               actionButton(inputId = "pop_two_selected", label = "Save group 2"),
                                                               br(),
                                                               div( id = 'definedInputSelection',
                                                                    checkboxInput(inputId = 'selectDefinedGroup',
                                                                                  value = F,
                                                                                  label = 'Select predefined cluster(s) for group 1'),
                                                                    checkboxGroupInput(inputId = 'whichGroups',
                                                                                       label = 'Predefined clusters:',
                                                                                       choices = unique(tsne$id))
                                                               ),
                                                               downloadButton(outputId = 'downloadDifGenes', label = 'Download'),
                                                               br(),
                                                               br(),
                                                               actionButton(inputId = "reload", label = "Select new groups")),
                                                      tabPanel('Create',
                                                               textInput(inputId = 'newClusterName',label  ='New name'),
                                                               h4(id = 'selectTextForRename', 'Please select group to rename'),
                                                               actionButton(inputId = 'renamePoulationButton',label = 'Rename Selection'),
                                                               checkboxInput(inputId = 'selectDefinedGroupForRename',
                                                                             value = F,
                                                                             label = 'Select predefined cluster(s)'),
                                                               checkboxGroupInput(inputId = 'whichGroupsForRename',
                                                                                  label = 'Predefined clusters:',
                                                                                  choices = unique(tsne$id))
                                                               )))),
                         column(9,
                         div(id= "div_select_one", plotlyOutput('tSNE_select_one',height = '600px')),
                         div(id = "div_select_two", plotlyOutput('tSNE_select_two',height = '600px')),
                         dataTableOutput('difGeneTable'),
                         div(id= 'comparisonOutput',

                             tabsetPanel(tabPanel('Visualize genes',
                                                  id = "histPlot",
                                                  plotlyOutput('histPlot', height = '500px')),
                                         tabPanel('Visualize selected groups',
                                                  id = 'tSNE plot',
                                                  plotlyOutput('tSNE_summary'), plotlyOutput('cell_type_summary')))
                             )
                         ))
               )
  )
)


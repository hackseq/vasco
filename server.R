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


source('difGenes.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # debugging output, modify at will -----
  output$debug <- renderPrint({ 
    #tsne[tsne$barcode %in% rownames(expression)[selected_vector1()],]
  })
  
  # main SNE plot ---------
  output$tSNEPlot <- renderPlotly({
    # size of the bins depend on the input 'bins'
    plot_ly(tsne, x = ~tSNE_1, y = ~tSNE_2, text = ~barcode, color = ~id, key = ~barcode, source= 'hede') %>%
      layout(dragmode = "select")
    
  })
  
  # COMPARE TAB-------
  output$tSNE_select <- renderPlotly({
    # size of the bins depend on the input 'bins'
    plot_ly(tsne, x = ~tSNE_1, y = ~tSNE_2, text = ~barcode, color = ~id, key = ~barcode) %>%
      layout(dragmode = "select")
  })
  #hide button one and two on load
  hide(id="plot_one_selected")
  #hide(id="plot_one_selected")
  
  # alternative plotting window after selection ------
  observeEvent(input$plot_selected, {
    updateTabsetPanel(session, "main_panel", selected = "Explore")
    
  })
  
  output$newPlot <- renderPlotly({
    input$plot_selected
    new_tsne <- isolate(selected_data())
    plot_ly(new_tsne, x = ~x, y = ~y, text = ~key) %>%
      layout(dragmode = "select")})
  
  # selection code and differential expression ------
  selected_data <- reactive({event_data("plotly_selected",source = 'hede')})
  
  selected_vector1 = reactive({
    tsneSubset = tsne[tsne$tSNE_1 %in% selected_data()$x & tsne$tSNE_2 %in% selected_data()$y,]
    print(table(tsneSubset$id))
    barcodes$Barcode %in% tsneSubset$barcode
  })
  selected_vector2 = reactive({!selected_vector1()})
  
  
  differentiallyExpressed = reactive({
    print('should I calculate dif genes?')
    if(!is.null(selected_data())){
      print('yeah I guess')
      difGenes(group1 = selected_vector1(), 
               group2 = selected_vector2())
    }
  })
  
  #observe({ write_tsv(differentiallyExpressed(), 'hede.tsv')})
  
  # dif genes heatmap
  output$difHeatmap = renderPlotly({
    print('rendering heatmap')
    if(!is.null(differentiallyExpressed())){
      orderedExpression  = expression[match(differentiallyExpressed()$Gene.Symbol,
                                            genes$Symbol),]
      print('expression ordered')
      # the selection is isolated so it will wait for differential expression results
      group1 = orderedExpression[,isolate(selected_vector1())]
      group2 = orderedExpression[,isolate(selected_vector2())]
      print(dim(group1))
      print(dim(group2))
      toPlot = cbind(group1,group2)
      print('plotted')
      plot_ly(z = toPlot[1:50,], x = '',
              y = differentiallyExpressed()$Gene.Symbol[1:50] %>% as.character,
              type = "heatmap")
    } else{
      print('or maybe not')
      NULL
    }
  })
  
  
  
  # histogram of cells -----------
  output$countPerCluster <- renderPlotly({
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    NumCells<-table(tsne$id)
    NumCells<-as.data.frame(NumCells)
    plot_ly(NumCells, x=~Var1, y=~Freq, color=~Var1, type='bar') %>%
      layout(xaxis = ax)
  })
  
  # plotting selected genes ----------
  output$geneExprPlot <- renderPlotly({
    #input_gene <- "CD8A"
    #gene_of_interest <- filter(genes, Symbol==input_gene)$ID
    gene_of_interest <- parse_gene_input(input$input_genes[1])
    gene_name <- parse_gene_input(input$input_genes[1], get="name")
    
    ## Pull out gene expression of gene of interest
    gene_expr <-
      data.frame(
        barcode = barcodes$Barcode,
        expr = expression[gene_of_interest,]) %>%
      tbl_df()
    ## Join with tSNE
    tsne1 <-
      left_join(tsne, gene_expr, by="barcode")
    ## Plot
    input_midplot <- 1
    tsne1 %>%
      ggplot(aes(x=tSNE_1, y=tSNE_2, color=expr)) +
      geom_point(alpha=1, size=.5) +
      scale_colour_gradient2(low="grey44", high="red", mid="grey99", midpoint=input_midplot) +
      theme_classic() +
      ggtitle(gene_name)
    ggplotly()
  })
  
  
})


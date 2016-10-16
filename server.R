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
  output$debug <- renderPrint({ #differentiallyExpressed()
  })
  
  # main SNE plot ---------
  output$tSNEPlot <- renderPlotly({
    # size of the bins depend on the input 'bins'
    plot_ly(tsne, x = ~tSNE_1, y = ~tSNE_2, text = ~barcode, color = ~id, key = ~barcode) %>%
      layout(dragmode = "select")
    
  })
  
  # COMPARE TAB-------
  #render initial seleciton plot
  output$tSNE_select_one <- renderPlotly({
    # size of the bins depend on the input 'bins'
    plot_ly(tsne, x = ~tSNE_1, y = ~tSNE_2, text = ~barcode, color = ~id, key = ~barcode, source = "selection_plot_one") %>%
      layout(dragmode = "select")
  })
  
  # selection code and differential expression ------
  selected_data <- reactive({event_data("plotly_selected", source = "selection_plot_one")})
  selected_data_two <- reactive({event_data("plotly_selected", source = "selection_plot_two")})
  
  
  observeEvent(selected_data(),{
    show("pop_one_selected")
  })
  
  observeEvent(selected_data_two(),{
    show("pop_two_selected")
  })
  
  #hide button one and two on load
  hide(id="pop_one_selected")
  hide(id="pop_two_selected")
  
  #render second selection plot
  output$tSNE_select_two <- renderPlotly({
    input$pop_one_selected
    isolate( plot_ly(tsne, x = ~tSNE_1, y = ~tSNE_2, text = ~barcode, color = ~id, key = ~barcode, source = "selection_plot_two") %>%
      layout(dragmode = "select") )
  })
  
  # alternative plotting window after selection ------
  observeEvent(input$pop_one_selected, {
    html(id = "select_text", "Please select second population")
    disable(id = "pop_one_selected")
  })
  
  #output$newPlot <- renderPlotly({
   # input$pop_selected
    #new_tsne <- isolate(selected_data())
    #plot_ly(new_tsne, x = ~x, y = ~y, text = ~key) %>%
     # layout(dragmode = "select")})
  
  
  selected_vector1 = reactive({
    barcodes$Barcode %in% selected_data()$key
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


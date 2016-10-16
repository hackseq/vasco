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
  #render initial seleciton plot
  output$tSNE_select_one <- renderPlotly({
    # size of the bins depend on the input 'bins'
    plot_ly(tsne, x = ~tSNE_1, y = ~tSNE_2, text = ~barcode, color = ~id, key = ~barcode, source = "selection_plot_one") %>%
      layout(dragmode = "select")
  })

  # selection code and differential expression ------
  selected_data <- reactive({event_data("plotly_selected", source = "selection_plot_one")})
  selected_data_two <- reactive({event_data("plotly_selected", source = "selection_plot_two")})

  #shows the button when first population selected in plot
  observeEvent(selected_data(),{
    show("pop_one_selected")
  })

  #shows the button when second population selected in plot
  observeEvent(selected_data_two(),{
    show("pop_two_selected")
  })

  observe({
  #hide button one and two on load
  hide(id="pop_one_selected")
  hide(id="pop_two_selected")
  #hide second plot on load
  hide(id="div_select_two")
  hide(id="comparisonOutput")
  })

  #render second selection plot when first population locked-in
  output$tSNE_select_two <- renderPlotly({
    input$pop_one_selected
    isolate( plot_ly(tsne, x = ~tSNE_1, y = ~tSNE_2, text = ~barcode, color = ~id, key = ~barcode, source = "selection_plot_two") %>%
      layout(dragmode = "select") )
  })

  # when button one is clicked, update ui and assign cell population to var
  observeEvent(input$pop_one_selected, {
    html(id = "select_text", "Please select second population")
    disable(id = "pop_one_selected")
    show(id= "div_select_two")
    hide(id="div_select_one")

  })

  # when button two is clicked, update ui and assign cell population to var
  observeEvent(input$pop_two_selected, {
    html(id = "select_text", "Loading...")
    disable(id = "pop_two_selected")
    show('comparisonOutput')
  })


  #output$newPlot <- renderPlotly({
   # input$pop_selected
    #new_tsne <- isolate(selected_data())
    #plot_ly(new_tsne, x = ~x, y = ~y, text = ~key) %>%
     # layout(dragmode = "select")})


  selected_vector1 = reactive(
    {input$pop_one_selected
      isolate({
        tsneSubset = tsne[tsne$tSNE_1 %in% selected_data()$x & tsne$tSNE_2 %in% selected_data()$y,]
        barcodes$Barcode %in% tsneSubset$barcode
      })})

  selected_vector2 = reactive(
    {
      hide('div_select_two')
      if(input$pop_two_selected == 1){
        isolate({
          tsneSubset = tsne[tsne$tSNE_1 %in% selected_data_two()$x & tsne$tSNE_2 %in% selected_data_two()$y,]
          barcodes$Barcode %in% tsneSubset$barcode
        })
      }
    }
  )


  differentiallyExpressed = reactive({
    print('should I calculate dif genes?')
      print('yeah I guess')
      if(!is.null(selected_vector2()) & !is.null(selected_vector1())){
        difGenes(group1 = isolate(selected_vector1()),
                 group2 = selected_vector2())
      }
  })


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


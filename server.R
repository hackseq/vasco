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
library(DT)

source('difGenes.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  rValues = reactiveValues(selected_vector1 = NULL,
                           selected_vector2 = NULL)

  #check if came from previous compare tab
  query_vals <- reactive({session$clientData$url_search
                                })

  observe({if ('?compare' ==query_vals()){updateTabsetPanel(session, inputId = 'main_panel', 'Compare') }})


  # main SNE plot ---------
  output$tSNEPlot <- renderPlotly({
    # size of the bins depend on the input 'bins'
    plot_ly(tsne, x = ~tSNE_1, y = ~tSNE_2, text = ~barcode, color = ~id, key = ~barcode, source= 'hede') %>%
      layout(
        dragmode = "select",
        xaxis = list(title = tsne_xlab),
        yaxis = list(title = tsne_ylab)
      )
  })

  # COMPARE TAB-------
  #render initial seleciton plot
  output$tSNE_select_one <- renderPlotly({
    # size of the bins depend on the input 'bins'
    plot_ly(tsne, x = ~tSNE_1, y = ~tSNE_2, text = ~barcode, color = ~id, key = ~barcode, source = "selection_plot_one") %>%
      layout(
        dragmode = "select",
        xaxis = list(title = tsne_xlab),
        yaxis = list(title = tsne_ylab)
      )
  })

  # selection code and differential expression ------
  selected_data <- reactive({event_data("plotly_selected", source = "selection_plot_one")})
  selected_data_two <- reactive({event_data("plotly_selected", source = "selection_plot_two")})

  #shows the button when first population selected in plot
  observe({
    if((((is.null(selected_data()) | is.null(dim(selected_data()))) & !input$selectDefinedGroup) |
       input$selectDefinedGroup & length(input$whichGroups)==0) | !is.null(rValues$selected_vector1)){
      disable("pop_one_selected")
    } else{
      enable('pop_one_selected')
    }
  })

  observe({
  #hide button one and two on load
  disable(id="pop_one_selected")
  hide(id="pop_two_selected")
  #hide second plot on load
  hide(id="div_select_two")
  hide(id="comparisonOutput")
  hide(id = 'downloadDifGenes')
  hide(id="reload")
  })


  #if select new groups button is pressed reload page on this tab
  observeEvent(input$reload, {
    print("hello worlddd")
    js$refresh()
  })

  #render second selection plot when first population locked-in
  output$tSNE_select_two <- renderPlotly({
    input$pop_one_selected
    isolate( plot_ly(tsne, x = ~tSNE_1, y = ~tSNE_2, text = ~barcode, color = ~id, key = ~barcode, source = "selection_plot_two") %>%
      layout(
        dragmode = "select",
        xaxis = list(title = tsne_xlab),
        yaxis = list(title = tsne_ylab)
      ))
  })

  # when button one is clicked, update ui and assign cell population to var
  # and show button two
  observeEvent(input$pop_one_selected, {
    html(id = "select_text", "Please select second population, or press save to select all outside group 1")
    disable(id = "pop_one_selected")
    show(id= "div_select_two")
    show("pop_two_selected")
    hide(id="div_select_one")
  })

  # when button two is clicked, update ui and assign cell population to var
  observeEvent(input$pop_two_selected, {
    html(id = "select_text", "Loading...")
    disable(id = "pop_two_selected")
    show('comparisonOutput')
    show('histPlot')
    show("reload")
  })

  #output$newPlot <- renderPlotly({
   # input$pop_selected
    #new_tsne <- isolate(selected_data())
    #plot_ly(new_tsne, x = ~x, y = ~y, text = ~key) %>%
     # layout(dragmode = "select")})


  # do I want to select defined groups?------
  observe({
    if(input$selectDefinedGroup){
      show(id = 'whichGroups')
    } else{
      updateCheckboxGroupInput(session, inputId = 'whichGroups', choices = unique(tsne$id), selected = NULL)
      hide(id = 'whichGroups')
    }
  })

  observe({
    if(input$pop_one_selected==1){
      isolate({
        updateCheckboxInput(session, inputId = 'selectDefinedGroup',
                            value = F,
                            label = 'Select defined groups?')
        print('group1 selection attempt')
        if(!input$selectDefinedGroup){
          tsneSubset = tsne[tsne$tSNE_1 %in% selected_data()$x & tsne$tSNE_2 %in% selected_data()$y,]
        } else{
          tsneSubset = tsne[tsne$id %in% input$whichGroups,]
        }
        rValues$selected_vector1 = barcodes$Barcode %in% tsneSubset$barcode
      })
    }
  })

  observe({
    if(input$pop_two_selected == 1){
      hide('div_select_two')
      hide(id = 'definedInputSelection')
      isolate({
        if(!input$selectDefinedGroup){
          if(!is.null(selected_data_two())){
            tsneSubset = tsne[tsne$tSNE_1 %in% selected_data_two()$x & tsne$tSNE_2 %in% selected_data_two()$y,]
            out = barcodes$Barcode %in% tsneSubset$barcode
          } else {
            # if nothing is selected, select the negative set based on tsne
            out = barcodes$Barcode %in% tsne$barcode[!tsne$barcode %in% barcodes$Barcode[rValues$selected_vector1]]
          }
        } else{
          tsneSubset = tsne[tsne$id %in% input$whichGroups,]
          out = barcodes$Barcode %in% tsneSubset$barcode
        }
        rValues$selected_vector2 = out
      })
    }
  })

  second_clicked_eds <-reactive({input$pop_two_selected
    g1 = tsne[ tsne$barcode %in% barcodes$Barcode[isolate({rValues$selected_vector1})],]
    g2 = tsne[tsne$barcode %in% barcodes$Barcode[isolate({rValues$selected_vector2})],]
    list(g1, g2)})




  second_clicked <-reactive({input$pop_two_selected})



  # Once group 1 and group 2 of cells are selected,
  # create 10 boxplots showing the gene expression distributions
  # of group 1 and group 2 for the top 10 up-regulated and
  # top 10 down-regulated genes
  output$histPlot <- renderPlotly({
    if ( !is.null(differentiallyExpressed()) ) {
      # TODO: Allow user to specify this
      gene_cnt <- 10
      
      nbr_group1 <- sum(rValues$selected_vector1)
      nbr_group2 <- sum(rValues$selected_vector2)
      nbr_barcodes <- nbr_group1 + nbr_group2
      
      diff_genes <- differentiallyExpressed()$`Gene Symbol`
      if(is.null(input$difGeneTable_rows_selected)){
        gene_indices <- c(1:gene_cnt, (length(diff_genes)-gene_cnt+1):length(diff_genes))
      } else{
        gene_indices = input$difGeneTable_rows_selected
      }
      
      dg_mat <- c()
      for ( n in gene_indices ) {
        # Get gene expression data and shift/log2-transform
        gene_idx <- which(genes$Symbol == diff_genes[n])
        dat1 <- log2(expression[gene_idx, rValues$selected_vector1] + 0.1)
        dat2 <- log2(expression[gene_idx, rValues$selected_vector2] + 0.1)
        
        # Store data into matrix of size 'nbr_barcodes' rows by 4 cols
        dg_mat <- rbind(dg_mat,
                        data.frame(gene = rep(diff_genes[n], nbr_barcodes),
                                   expr = c(dat1, dat2),
                                   group = c(rep("1", nbr_group1),
                                             rep("2", nbr_group2)),
                                   panel = rep(n, nbr_barcodes)
                        )
        )
      }
      
      # Ensure that data type for each column is appropriate for ggplot display
      # TODO: Simplify this...
      dg_mat <-
        dg_mat %>% mutate(
          gene = as.character(gene),
          expr = as.numeric(expr),
          group = as.factor(group),
          panel = as.factor(panel)) %>%
        arrange(panel)
      
      # TODO: Find a better way to preserve gene order
      dg_mat$gene <- factor(dg_mat$gene, levels = dg_mat$gene)
      
      ggplot(dg_mat, aes(x=group, y=expr, fill=group)) + geom_boxplot() +
        facet_wrap(~gene, scales="free_x", nrow=2, ncol=gene_cnt) +
        theme(panel.margin = unit(1, "lines"),
              panel.background = element_rect(fill = "white"),
              strip.background = element_rect(fill = "white"),
              legend.position = "none")
      ggplotly()
    } else {
      plotly_empty()
    }
  })
  
  output$tSNE_summary <- renderPlotly({
    groups <- second_clicked_eds()
    g1 = groups[[1]]
    g2 = groups[[2]]
    g1["group"] <- rep('group 1', dim(g1)[1])
    g2["group"] <- rep('group 2', dim(g2)[1])
    both_groups = rbind(g1, g2)
    plot_ly(both_groups, x = ~tSNE_1, y = ~tSNE_2, text = ~barcode, color = ~group, colors = c("dark blue", "dark red"),
            key = ~barcode, source = "selection_plot_two") %>%
               layout(dragmode = "select",xaxis = list(range = c(-40,40)),
                      yaxis = list(range = c(-40,40)))
    })

  # histogram of cells -----------
  output$cell_type_summary <- renderPlotly({
    tsne_id <- table(tsne$id)
    categories<- dim(tsne_id)
    #make dummy array of all types of tsne clusters so that tables() returns an entry for each type
    dummy = data.frame(rep('AAAAAAAAAAAA', 8), rep(1.0, 8), rep(1.0, 8), names(tsne_id))
    names(dummy) = names(tsne)
    groups <- second_clicked_eds()
    g1 <-  rbind(groups[[1]], dummy)
    g2 <-  rbind(groups[[2]], dummy)
    #subtract 1 because we added an extra entry of each type in dummy array
    g1_cell_counts<-table(g1$id) - 1
    g2_cell_counts<-table(g2$id) - 1
    cell_names <- names(g1_cell_counts)
    data <- as.data.frame(rbind(g1_cell_counts, g2_cell_counts))
    plot_ly(data, x=cell_names, y=~g1_cell_counts, type='bar', name = 'group 1') %>%
      add_trace(y=~g2_cell_counts, name = "group 2") %>%
      layout( yaxis = list(title = 'Count'), barmode = 'group')
  })



  output$downloadDifGenes = downloadHandler(
    filename = 'difGenes.tsv',
    content = function(file) {
      write_tsv(differentiallyExpressed(), file)
    })


  differentiallyExpressed = reactive({
    print('should I calculate dif genes?')
      print('yeah I guess')
      if(!is.null(rValues$selected_vector2) & !is.null(rValues$selected_vector1)){
        show('downloadDifGenes')
        difGenes(group1 = isolate(rValues$selected_vector1),
                 group2 = rValues$selected_vector2)
      }
  })


  output$difGeneTable = renderDataTable({
    if(!is.null(differentiallyExpressed())){
      datatable(differentiallyExpressed(),selection = 'multiple')
    }
  })
  # if a gene is selected from the data table, select that gene in the expression window
  observe({
    if(!is.null(input$difGeneTable_rows_selected)){
      gene = differentiallyExpressed()[input$difGeneTable_rows_selected,]$`Gene Symbol`
      selectedGene = list_of_genesymbols[grepl(regexMerge(paste0('^',gene,'_')),list_of_genesymbols)]
      updateSelectInput(session, 'input_genes', selected = selectedGene)
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
      layout(xaxis = ax,
             yaxis = list(title = "Number of cells"))
  })

  # plotting selected genes ----------
  # disable button when empty
  observe({
    if(length(input$input_genes)==0 ){
      disable('exprGeneButton')
    }else{
      enable('exprGeneButton')
    }
  })

  geneExpr_genes <- reactive({
    # Take a dependency on input$goButton
    input$exprGeneButton
    input$difGeneTable_rows_selected
    print('drawing gene plots')

    isolate(input$input_genes)
    })
  output$geneExprPlot <- renderUI({
    plot_output_list <- lapply(1:length(geneExpr_genes()), function(i) {
      plotname <- paste("plot", i, sep="")
      plotlyOutput(plotname)
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })

  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  for (i in 1:geneExpr_maxItems) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")

      output[[plotname]] <- renderPlotly({
        gene_of_interest <- parse_gene_input(geneExpr_genes()[my_i])
        gene_name <- parse_gene_input(geneExpr_genes()[my_i], get="name")
        plot_geneExpr(gene_of_interest, gene_name,
                      value_rangemid=input$Midpoint,
                      value_min = input$MinMax[1],
                      value_max = input$MinMax[2],
                      color_low = input$colmin,
                      color_mid = input$colmid,
                      color_high = input$colmax)
      })
    })
  }

  output$geneExprGeneCluster <- renderPlotly({
    gene_of_interest <- parse_gene_input(geneExpr_genes())
    gene_name <- parse_gene_input(geneExpr_genes(), get="name")
    plot_geneExprGeneCluster(gene_of_interest, gene_name)
  })


}
)

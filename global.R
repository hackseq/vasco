library(readr)
library(dplyr)
library(Matrix)
library(plotly)
library(magrittr)

barcodes = read_tsv('Data/redstone_1_barcodes.tsv', col_names = 'Barcode')
genes = read_tsv('Data/redstone_1_genes.tsv',col_names = c('ID','Symbol'))
tsne = read_tsv('Data/redstone_pbmc3k_tdf', skip= 1, col_name = c('barcode','tSNE_1',	'tSNE_2','id' ))
expression = readMM('Data/redstone_1_matrix.mtx')

geneExpr_maxItems = 4
rownames(expression) = genes$ID
colnames(expression) = barcodes$Barcode

# get rid of genes (aka. rows) for which all cells have expression = 0
rowMax <- expression %>% apply(1,max)
expression <- expression[rowMax>0,]
genes <- genes[rowMax > 0,]

normalizeExpression = function(mat) {
 mat<-expression
 mat<-t(mat)
 
 # for each cell, compute total expression
 expression_sum_for_each_cell <- rowSums(mat)
 # get the overall median expression value
 overall_median_expression <- median(expression_sum_for_each_cell)
 
 normalized_expression <- mat/(expression_sum_for_each_cell/overall_median_expression)
 t(normalized_expression)
}

expression = normalizeExpression(expression)

genes$Symbol_ID <- paste(genes$Symbol,genes$ID, sep="_")
list_of_genesymbols <- sort(genes$Symbol_ID)

#' Normalization
parse_gene_input <- function(x, get="id"){
  if (get=="name") {
    gsub("(^.*)_ENSG\\d+", "\\1", x)
  } else {
    gsub("^.*_(ENSG\\d+)", "\\1", x)
  }
}

#' Draw geneExpr scatterplot
plot_geneExpr <- function(gene_of_interest, gene_name, input_midplot=1,
                          color_low="grey44", color_mid="grey99", color_high="red"){
  gene_expr <-
    data.frame(
      barcode = barcodes$Barcode,
      expr = expression[gene_of_interest,]) %>%
    tbl_df()
  ## Join with tSNE
  tsne1 <-
    left_join(tsne, gene_expr, by="barcode")
  ## Plot
  tsne1 %>%
    ggplot(aes(x=tSNE_1, y=tSNE_2, color=expr)) +
    geom_point(alpha=1, size=.5) +
    scale_colour_gradient2(low=color_low,
                           mid=color_mid,
                           high=color_high,
                           midpoint=input_midplot) +
    theme_classic() +
    ggtitle(gene_name)
  ggplotly()
}

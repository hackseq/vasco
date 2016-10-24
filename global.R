print("ok let''s start this")
library(readr)
library(dplyr)
library(tidyr)
library(Matrix)
library(plotly)
library(magrittr)
library(qlcMatrix)
source('regexMerge.R')

#for file upload
options(shiny.maxRequestSize=30*1024^2) 
barcodes = read_tsv('Data/redstone_1_barcodes.tsv', col_names = 'Barcode')
genes = read_tsv('Data/redstone_1_genes.tsv',col_names = c('ID','Symbol'))
tsne = read_tsv('Data/redstone_pbmc3k_tdf', skip= 1,
                col_name = c('barcode','tSNE_1', 'tSNE_2','cluster_id', 'id'),
                col_types = cols(id = col_character())
                )
# tsne = read_tsv('Data/redstone_pbmc3k_tdf', skip= 1,
#                 col_name = c('barcode','tSNE_1', 'tSNE_2','id'))
print('expression data read')
expression = readMM('Data/redstone_1_matrix.mtx')
print('set rownames')
rownames(expression) = genes$ID
print('set colnames')
colnames(expression) = barcodes$Barcode
print('data reading complete')

tsne_xlab <- "TSNE 1"
tsne_ylab <- "TSNE 2"

geneExpr_maxItems = 4
geneExpr_colorMin = "#EAF7F7"
geneExpr_colorMax = "#FF00EA"
geneExpr_colorMid <- "#B8C1D6"
print('initialized parameters')

#colorRampPalette(c(geneExpr_colorMin, geneExpr_colorMax))(4)[2]

# get rid of genes (aka. rows) for which all cells have expression = 0
rowMax <- expression %>% (qlcMatrix::rowMax)
print('rowmax calculated')
expression <- expression[(rowMax>0) %>% as.logical,]
print('genes removed from expression matrix')
genes <- genes[(rowMax>0) %>% as.logical,]
print('genes removed from genes')
print('non expressed genes removed')

normalizeExpresion = function(v) {
  # for each cell, compute total expression
  expression_sum_for_each_cell <- colSums(expression)
  # get the overall median expression value
  overall_median_expression <- median(expression_sum_for_each_cell)
  # scale each expression value by the cell-specific scale factor
  scale_factor_for_each_cell <- (expression_sum_for_each_cell/overall_median_expression)
  normalized_expression <- expression/scale_factor_for_each_cell

  normalized_expression
}

expression = normalizeExpresion(expression)

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
plot_geneExpr <- function(gene_of_interest, gene_name,
                          value_min = 0, value_max = 1, value_rangemid = 0.5,
                          color_low = "grey99", color_mid= "grey44", color_high = "red"){
  gene_expr <-
    data.frame(
      barcode = barcodes$Barcode,
      expr = expression[gene_of_interest,]) %>%
    tbl_df()

  max_expr <- max(gene_expr$expr)
  minval <- max_expr*value_min
  maxval <- max_expr*value_max
  midval <- (((maxval-minval)*value_rangemid)+minval)

  ## Join with tSNE
  tsne1 <-
    left_join(tsne, gene_expr, by="barcode")
  ## Plot
  tsne1 %>%
    ggplot(aes(x=tSNE_1, y=tSNE_2, color=expr)) +
    geom_point(alpha=1, size=.5) +
    scale_color_gradientn(
      colours = c(color_low, color_mid, color_high),
      values = c(0, minval, midval, maxval, max_expr),
      rescaler = function(x, ...) x, oob = identity
    ) +
    xlab(tsne_xlab) +
    ylab(tsne_ylab) +
    theme_classic() +
    ggtitle(gene_name)
  ggplotly()
}

#' Draw geneExpr boxplot by cluster
plot_geneExprGeneCluster <- function(gene_of_interest, gene_name,tsne){
  the_genes <- setNames(gene_name, gene_of_interest)

  if (length(gene_of_interest) == 1) {
    gene_expr <-
      data.frame(
        barcode = barcodes$Barcode,
        expr = expression[gene_of_interest,]) %>%
      tbl_df()
    colnames(gene_expr)[2] <- gene_of_interest
  } else {
    gene_expr <-
      expression[names(the_genes),] %>%
      as.matrix() %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column("barcode")
  }
  tsne1 <-
    left_join(tsne, gene_expr, by="barcode") %>%
    gather(gene, expr, starts_with("ENSG")) %>%
    mutate(
      gene = as.factor(gene),
      gene = plyr::revalue(gene, the_genes)
    )
  tsne1 %>%
    ggplot(aes(x=id, y=expr)) +
    geom_boxplot() +
    facet_wrap(~gene) +
    xlab("") +
    ylab("Normalized expression") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
      strip.background = element_rect(fill = NA, colour = NA)
    )
  ggplotly()
}

print('global excecuted')

library(readr)
library(dplyr)
library(Matrix)
library(plotly)
library(magrittr)

barcodes = read_tsv('Data/redstone_1_barcodes.tsv', col_names = 'Barcode')

genes = read_tsv('Data/redstone_1_genes.tsv',col_names = c('ID','Symbol'))
tsne = read_tsv('Data/redstone_pbmc3k_tdf', skip= 1, col_name = c('barcode','tSNE_1',	'tSNE_2','id' ))
expression = readMM('Data/redstone_1_matrix.mtx')

rowMax = expression %>% apply(1,max)
expression = expression[rowMax>0,]
genes = genes[rowMax > 0,]

normalizeExpresion = function(v) {
  expression %>% apply(1,function(v){
    m <- log(1+v)
    m<-m-mean(m)
    m<-m/sd(m)
    return(m)
  }) %>% t

}

expression = normalizeExpresion(expression)

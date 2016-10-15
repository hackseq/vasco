difGenes = function(group1,group2, geneCount = 10){
  group1 = expression[,group1]
  group2 = expression[,group2]

  group1Median = group1 %>% apply(1,mean)
  group1Median = group2 %>% apply(1,mean)
  
  fChange = group1Median - group2Median
  
  difGeneOrder = order(fChange, decreasing = TRUE)
  data.frame(Gene.Symbol = genes$Symbol[difGeneOrder][1:geneCount],
             difference = fChange[difGeneOrder][1:geneCount],
             group1Expression = group1Median[difGeneOrder][1:geneCount],
             group2Expression = group1Median[difGeneOrder][1:geneCount])

  
}
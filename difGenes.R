difGenes = function(group1,group2){

  group1 = expression[,group1]
  group2 = expression[,group2]

  group1Mean = rowMeans(group1)
  group2Mean = rowMeans(group2)

  fChange = (group1Mean+0.1) / (group2Mean+0.1)

  difGeneOrder = order(fChange, decreasing = TRUE)
  data.frame(Gene.Symbol = genes$Symbol[difGeneOrder],
             difference = fChange[difGeneOrder],
             group1Expression = group1Mean[difGeneOrder],
             group2Expression = group2Mean[difGeneOrder])

}
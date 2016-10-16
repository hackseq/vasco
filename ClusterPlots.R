# Violin plots etc


#TEST CODE
library(plotly)
plot_ly(y = ~normalize_expr, type = "violin") %>%
  add_trace(y = ~rnorm(50, 1))

#FROM GRACE: makes violin plot
ggplot(df,aes(factor(group),y))+geom_violin()

#NEED TO
# - create dataframe df with 2 columns: y and Group
# - search matrix for row (gene) and look for cells corresponding to clusters of interest

df<-data.frame()

id2 = subset(tsne, id == "B cells")$barcode
id3 = subset(tsne, id == "NK cells")$barcode

df[1,] = expression["GENE1", id2]  #only takes row if the row is equal to the input gene (MAX input 4 genes)
df[2,] = expression["GENE2", id2]
df[3,] = expression["GENE3", id2]
df[4,] = expression["GENE4", id2]

#dfnew<-data.frame[]
#dfnew<-dfnew(df[,"group"] == GROUPSELECTED) #only pulls columns of cell type of interest
#names(dfnew)<-("y", "group")


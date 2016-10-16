# Violin plots etc

install.packages("reshape")
library(reshape)

#NEED TO
# - create dataframe df with 2 columns: y and Group
# - search matrix for row (gene) and look for cells corresponding to clusters of interest

df<-data.frame()

id2 = subset(tsne, id == "B cells")$barcode
id3 = subset(tsne, id == "NK cells")$barcode

selectedgenes = c("ENSG00000171798", "ENSG00000151651", "ENSG00000161328", "ENSG00000180066")
df2 = expression[selectedgenes, id2]
df2 <- as.matrix(df2)
df2 <- t(df2)
df2 <- melt(df2, id=c("geneid", "value"))
ggplot(df2,aes(factor(df2$X2),df2$value))+geom_violin()

selectedgenes = c("ENSG00000171798", "ENSG00000151651", "ENSG00000161328", "ENSG00000180066")
df3 = expression[selectedgenes, id3]
df3 <- as.matrix(df3)
df3 <- t(df3)
df3 <- melt(df3, id=c("geneid", "value"))
ggplot(df3,aes(factor(df3$X2),df3$value))+geom_violin()

dfnew = (df[1,])
#df[1,] = expression["ENSG00000171798", id2]  #only takes row if the row is equal to the input gene (MAX input 4 genes)
#df[2,] = expression["ENSG00000151651", id2]
#df[3,] = expression["ENSG00000161328", id2]
#df[4,] = expression["ENSG00000180066", id2]

#dfnew<-data.frame[]
#dfnew<-dfnew(df[,"group"] == GROUPSELECTED) #only pulls columns of cell type of interest
#names(dfnew)<-("y", "group")


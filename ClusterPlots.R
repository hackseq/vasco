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

selectedgenes = c("ENSG00000171798", "ENSG00000151651", "ENSG00000161328", "ENSG00000180066")
df3 = expression[selectedgenes, id3]
df3 <- as.matrix(df3)
df3 <- t(df3)
df3 <- melt(df3, id=c("geneid", "value"))

plot2 <- ggplot(df2,aes(factor(df2$X2),df2$value))+geom_violin()
plot3 <- ggplot(df3,aes(factor(df3$X2),df3$value))+geom_violin()
subplot(plot2, plot3)


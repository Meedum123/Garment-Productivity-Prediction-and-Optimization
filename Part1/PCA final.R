library(ggplot2)

data=read.csv("train_cleandata.csv")

data=data[,c("targeted_productivity","smv","wip",
             "over_time","incentive","idle_time",
             "idle_men","no_of_style_change",
             "no_of_workers","actual_productivity")]

pca_result <- prcomp(data, center = TRUE, scale. = TRUE)

summary(pca_result)

screeplot(pca_result, type = "lines", main = "Scree Plot of PCA")

pca_loadings=pca_result$rotation[, 1:2]  # Loadings for PC1 and PC2
pca_scores <- pca_result$x # scores

head(pca_loadings)  
###### PCA loadings plot
plot(pca_loadings[, 1], pca_loadings[, 2],  
     xlab = "PC1", ylab = "PC2",  
     main = "PCA Loadings Plot",  
     pch = 19, col = "blue",  
     cex = 1.2,   
     xlim = range(c(-0.6,0.5)), 
     ylim = range(pca_loadings[,2]) * 1.2)

grid()

text(pca_loadings[,1], pca_loadings[,2], 
     labels = rownames(pca_loadings), 
     pos = 4, cex = 0.9, col = "red")

arrows(0, 0, pca_loadings[,1], pca_loadings[,2], 
       length = 0.1, col = "darkgray", lwd = 1.5)

#######
plot(pca_scores[, 1], pca_scores[, 2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA Score Plot", pch = 19, col = "blue")
grid()
abline(h=0)
abline(v=0)

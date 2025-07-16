library('FactoMineR')
library(Factoshiny)
library(cluster)     

res <- Factoshiny(train_data)
cor(train_data$YearsInMostRecentRole,train_data$YearsSinceLastPromotion)


# Perform FAMD
famd_result <- FAMD(train_data, graph = FALSE, ncp = 40)

# Get the explained variance of components
eig_values <- famd_result$eig[,2]  # Percentage of variance explained by each component
cumulative_variance <- cumsum(eig_values)  # Cumulative sum of variance

# Find the number of components explaining at least 80% variance
num_components <- which(cumulative_variance >= 70)[1]

# Extract the principal components
famd_components <- famd_result$ind$coord[, 1:num_components]
# Run k-means clustering
set.seed(123)  # For reproducibility
s_val=c()
for(i in 2:10){
  clus <- kmeans(famd_components, centers = i, 100)
  
  # Compute silhouette scores
  sil <- silhouette(clus$cluster, dist(famd_components))
  
  # Print average silhouette width
  cat("i",i,"/nAverage Silhouette Width:", mean(sil[, 3]), "\n")
  #fviz_silhouette(sil)
  s_val=c(s_val, mean(sil[, 3]))
}

clus <- kmeans(famd_components, centers = 2, 100)

# Compute silhouette scores
sil <- silhouette(clus$cluster, dist(famd_components))

# Print average silhouette width
cat("i",i,"/nAverage Silhouette Width:", mean(sil[, 3]), "\n")
fviz_silhouette(sil)

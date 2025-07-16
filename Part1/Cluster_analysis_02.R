library('FactoMineR')
library(cluster)       # For silhouette()
library(factoextra)    # For fviz_silhouette()
data=read.csv("train_cleandata.csv")

response="actual_productivity"
numeric_vars <- data[,c("targeted_productivity","smv","wip",
                        "over_time","incentive","idle_time",
                        "idle_men","no_of_style_change",
                        "no_of_workers")]
productivity_state=as.factor(as.numeric(data$actual_productivity>=data$targeted_productivity))
categorical_vars <- data[, c("date","quarter", "day","team","department")]
categorical_vars=cbind(categorical_vars, productivity_state)
categorical_vars <- data.frame(lapply(categorical_vars, as.factor))
comb_data=cbind(categorical_vars, numeric_vars)
head(comb_data)

# Perform FAMD
famd_result <- FAMD(comb_data, graph = FALSE, ncp = 2)

# Get the explained variance of components
eig_values <- famd_result$eig[,2]  # Percentage of variance explained by each component
cumulative_variance <- cumsum(eig_values)  # Cumulative sum of variance

# Find the number of components explaining at least 80% variance
# num_components <- which(cumulative_variance >= 80)[1]

# Extract the principal components
# famd_components <- famd_result$ind$coord[, 1:num_components]  # Select first 'num_components' PCs
famd_components <- famd_result$ind$coord[, 1:2]
# Run k-means clustering
set.seed(123)  # For reproducibility
for(i in 2:10){
  clus <- kmeans(famd_components, centers = i, 100)
  
  # Compute silhouette scores
  sil <- silhouette(clus$cluster, dist(famd_components))
  
  # Print average silhouette width
  cat("i",i,"/nAverage Silhouette Width:", mean(sil[, 3]), "\n")
  #fviz_silhouette(sil)
}

clus <- kmeans(famd_components, centers = 2, 100)

# Compute silhouette scores
sil <- silhouette(clus$cluster, dist(famd_components))


# Print average silhouette width
cat("i",i,"/nAverage Silhouette Width:", mean(sil[, 3]), "\n")
fviz_silhouette(sil)

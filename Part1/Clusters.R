library('FactoMineR')
data=read.csv("train_cleandata.csv")

numeric_vars <- data[,c("targeted_productivity","smv","wip",
                        "over_time","incentive","idle_time",
                        "idle_men","no_of_style_change",
                        "no_of_workers","actual_productivity")]
productivity_state=as.factor(as.numeric(data$actual_productivity>=data$targeted_productivity))
categorical_vars <- data[, c("date","quarter", "day","team","department")]
categorical_vars=cbind(categorical_vars, productivity_state)
categorical_vars <- data.frame(lapply(categorical_vars, as.factor))
head(categorical_vars)
mca_result <- MCA(categorical_vars, ncp = 5, graph = FALSE)
mca_transformed <- mca_result$ind$coord  # Extract transformed quantitative values
head(mca_result$var$contrib)


comb_data=cbind(mca_transformed, numeric_vars)
head(comb_data)
comb_data_scaled <- scale(comb_data)
######### kmeans
clus=kmeans(comb_data,2,100)

library(cluster)       # For silhouette()
library(factoextra)    # For fviz_silhouette()

# Run k-means clustering
set.seed(123)  # For reproducibility
for(i in 2:10){
  clus <- kmeans(comb_data, centers = i, 100)
  
  # Compute silhouette scores
  sil <- silhouette(clus$cluster, dist(comb_data))
  
  # Print average silhouette width
  cat("i",i,"/nAverage Silhouette Width:", mean(sil[, 3]), "\n")
  #fviz_silhouette(sil)
}

clus <- kmeans(comb_data, centers = 2, 100)

# Compute silhouette scores
sil <- silhouette(clus$cluster, dist(comb_data))


# Print average silhouette width
cat("i",i,"/nAverage Silhouette Width:", mean(sil[, 3]), "\n")
fviz_silhouette(sil)

chisq.test(as.factor(data$department), as.factor(clus$cluster))

library(rcompanion)
cramerV(as.factor(data$department), as.factor(clus$cluster))
# cramerV value is 0.67 therefore department is the main reason for clustering

#ARI <- adjustedRandIndex(clus$cluster, data$department) # adjustedRandIndex
#ARI

chisq.test(as.factor(data$department), as.factor(clus$cluster))

cramerV(as.factor(data$department), as.factor(clus$cluster))

#########
cla_acr=table(clus$cluster, data$department)
cla_acr

ac=sum(diag(cla_acr))/sum(cla_acr)

# ac is 0.82 therefore main reason for cluster is department


(392+397)/sum(cla_acr)

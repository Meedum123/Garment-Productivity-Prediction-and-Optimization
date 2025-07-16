data=read.csv("train_cleandata.csv")

library(FactoMineR)
numeric_vars <- data[,c("targeted_productivity","smv","wip",
                        "over_time","incentive","idle_time",
                        "idle_men","no_of_style_change",
                        "no_of_workers","actual_productivity")]
categorical_vars <- data[, c("date","quarter", "department", "day", "team")]  # Replace with actual categorical columns
categorical_vars <- data.frame(lapply(categorical_vars, as.factor))
mca_result <- MCA(categorical_vars, graph = FALSE)
mca_transformed <- mca_result$ind$coord  # Extract transformed quantitative values
print(mca_result$var$contrib)

comb_data=cbind(mca_transformed, numeric_vars)
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

#####departmet is the case??

library(mclust)  # For adjustedRandIndex() and mutualInfo()

# Assuming `cluster_labels` and `department_labels` are vectors
tt=table(clus$cluster, data$department)
accuracy=sum(diag(tt)/sum(tt))

# Compute Adjusted Rand Index (ARI)
ARI <- adjustedRandIndex(clus$cluster, data$department) # adjustedRandIndex

# Compute Normalized Mutual Information (NMI)
nmi <- mutualInfo(clus$cluster, data$department, normalized = TRUE)

# Print results
cat("Adjusted Rand Index (ARI):", ARI, "\n")
cat("Normalized Mutual Information (NMI):", nmi, "\n")

# what are the most influential to clusters

# Add cluster labels to your data
data$cluster <- clus$cluster

# Loop through numeric variables and test for differences across clusters
numeric_vars <- c("targeted_productivity", "smv", "wip", "over_time", "incentive", 
                  "idle_time", "idle_men", "no_of_style_change", "no_of_workers", "actual_productivity")

for (var in numeric_vars) {
  # ANOVA (if assumptions met) or Kruskal-Wallis (non-parametric)
  res <- kruskal.test(data[[var]] ~ as.factor(data$cluster))
  cat(paste0(var, ": p-value = ", round(res$p.value, 4), "\n"))
}


########################
cccc=factor(clus$cluster)
boxplot(data$actual_productivity ~ cccc, main = "Productivity by Cluster", 
        xlab = "Cluster", ylab = "Actual Productivity", col = c("red", "blue", "green"))

result <- wilcox.test(data$actual_productivity ~ factor(clus$cluster))
result
# p-val<0.05 there is a significant difference

median(data$actual_productivity[clus$cluster==2]) 
median(data$actual_productivity[clus$cluster==1]) 

####
shapiro.test(data$actual_productivity)

hist(log(data$actual_productivity))
hist(1/(data$actual_productivity))
hist(sqrt(data$actual_productivity))
hist((data$actual_productivity)^(1/3))

library(MASS)

# Run Box-Cox and store the result
bcox <- boxcox(data$actual_productivity ~ 1, lambda = seq(-10, 10, 0.1))

# Find the best lambda (where log-likelihood is highest)
best_lambda <- bcox$x[which.max(bcox$y)]
print(best_lambda)

if (best_lambda == 0) {
  data$transformed_productivity <- log(data$actual_productivity)  # Log transform if λ = 0
} else {
  data$transformed_productivity <- (data$actual_productivity^best_lambda - 1) / best_lambda
}

hist(data$transformed_productivity, 
     main = "Histogram of Transformed Productivity",
     xlab = "Transformed Productivity", 
     col = "lightblue", 
     border = "black")

shapiro.test(data$transformed_productivity)

# Mann - whiney
result = wilcox.test(data$actual_productivity ~ cccc)

##########
ggplot(data, aes(x = cccc, y = department, color = cccc)) +
  geom_point() + theme_minimal()

ggplot(data, aes(x = department, fill = cccc)) +
  geom_bar(position = "dodge") + 
  theme_minimal() + 
  labs(title = "Department Distribution by Cluster", x = "Department", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(data$department, cccc), margin = 2)  # Proportion within each cluster

library(ggmosaic)
ggplot(data) + 
  geom_mosaic(aes(weight = 1, x = product(department, cccc), fill = cccc)) + 
  labs(title = "Department vs. Cluster")

##################
pairs(cbind(data$actual_productivity,data$no_of_workers), col=clus$cluster)

pro_status=(data$targeted_productivity<data$actual_productivity)

pro_dep=table(pro_status,data$department)
pro_clus=table(pro_status,clus$cluster)

sum(diag(pro_dep))/sum(pro_dep)
sum(diag(pro_clus))/sum(pro_clus)

head(data[(data$date=="2015-02-18" & data$team==6),])

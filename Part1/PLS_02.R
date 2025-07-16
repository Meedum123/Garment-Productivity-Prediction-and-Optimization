library(FactoMineR)   # For MCA
library(pls)          # For PLS regression
library(ggplot2)      # For visualization

data <- read.csv("train_cleandata.csv")

categorical_vars <- data[, c("quarter", "department", "day", "team")]  # Replace with actual categorical columns
categorical_vars <- data.frame(lapply(categorical_vars, as.factor))
mca_result <- MCA(categorical_vars, graph = FALSE)
mca_transformed <- mca_result$ind$coord  # Extract transformed quantitative values
print(mca_result$var$contrib)

correlation_matrix <- data[,c("targeted_productivity","smv","wip",
                                  "over_time","incentive","idle_time",
                                  "idle_men","no_of_style_change",
                                  "no_of_workers","actual_productivity")]  # Select numeric columns
processed_data <- cbind(numeric_vars, mca_transformed)

pls_model <- plsr(actual_productivity ~ ., data = processed_data, scale = TRUE, validation = "CV")

scores <- data.frame(pls_model$scores[, 1:2])
colnames(scores) <- c("Component1", "Component2")

ggplot(scores, aes(x = Component1, y = Component2)) +
  geom_point(alpha = 0.6, color = "blue") +
  theme_minimal() +
  labs(title = "PLS Regression: First vs. Second Component",
       x = "Component 1",
       y = "Component 2")


library(mdatools)

Xc=processed_data[,-11]
Yc=processed_data[,11]
Model1<-pls(Xc,Yc,scale = TRUE,cv=1,
            info = "prediction model")
#Model summary
summary(pls_model)
# bias - bias for prediction vs. measured values
# Residual Prediction Deviation (RPD)= sd of observed/RMSEP
plot(Model1)
plotXScores(Model1,show.labels = TRUE)
plotXYLoadings(Model1,show.labels =TRUE)



######         PLS  using pls library            ###########

pls_model <- plsr(actual_productivity ~ ., data = data, scale = TRUE, validation = "CV")
summary(pls_model)
pls_model$ncomp

lo=pls_model$loadings[,1:2]
#plotXYLoadings(pls_model,show.labels = TRUE)
plot(lo[,1],lo[,2])


library(mdatools)
library(pls)
set.seed(2)

data=read.csv("train_cleandata.csv")

data=data[,c("targeted_productivity","smv","wip",
             "over_time","incentive","idle_time",
             "idle_men","no_of_style_change",
             "no_of_workers","actual_productivity")]

pcr.fit<-pcr(actual_productivity~., data=data ,scale=TRUE, center=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit ,val.type="RMSEP")
plot(RMSEP(pcr.fit), legendpos = "topright")

pcr.fit2<-pcr(Salary~.,data=Hitters,scale=TRUE,ncomp=5) 
summary(pcr.fit2)
coef(pcr.fit2)

plotXYLoadings(pcr.fit,show.labels =TRUE) 

lo=pcr.fit$loadings[,1:2]
plotXYLoadings(pcr.fit,show.labels = TRUE)
plot(lo[,1],lo[,2])

lo <- pcr.fit$loadings[, 1:2] # Extract first two principal component loadings
plot(lo[,1], lo[,2], 
     xlab = "PC1 Loadings", 
     ylab = "PC2 Loadings", 
     main = "Loadings Plot", 
     pch = 19)  # Solid circles

# Add labels to points
text(lo[,1], lo[,2], labels = rownames(lo), pos = 4, cex = 0.8, col = "blue")


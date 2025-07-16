library(FactoMineR)   # For MCA
library(pls)          # For PLS regression
library(ggplot2)      # For visualization

data <- read.csv("train_cleandata.csv")

categorical_vars <- data[, c("quarter", "department", "day", "team")]  # Replace with actual categorical columns
categorical_vars <- data.frame(lapply(categorical_vars, as.factor))
mca_result <- MCA(categorical_vars, graph = FALSE)
mca_transformed <- mca_result$ind$coord  # Extract transformed quantitative values
print(mca_result$var$contrib)

numeric_vars <- data[,c("targeted_productivity","smv","wip",
                              "over_time","incentive","idle_time",
                              "idle_men","no_of_style_change",
                              "no_of_workers","actual_productivity")]  # Select numeric columns
processed_data <- cbind(numeric_vars, mca_transformed)

library(mdatools)

Xc=processed_data[,-10]
Yc=processed_data[,10]
head(Xc)
Model1<-pls(Xc,Yc,scale = TRUE,cv=1,
            info = "prediction model")
#Model summary
summary(pls_model)
# bias - bias for prediction vs. measured values
# Residual Prediction Deviation (RPD)= sd of observed/RMSEP
plot(Model1)
plotXScores(Model1,show.labels = TRUE)
plotXYLoadings(Model1,show.labels =TRUE)

# Importing the library mdatools
library(mdatools)

data=read.csv("train_cleandata.csv")

data=data[,c("targeted_productivity","smv","wip",
             "over_time","incentive","idle_time",
             "idle_men","no_of_style_change",
             "no_of_workers","actual_productivity")]

Xc=data[,-10]
yc=data[,10]
#Fitting PLR model
Model1<-pls(Xc, yc, scale = TRUE,cv=1,
            info = "Shoesize prediction model")
#Model summary
summary(Model1)
# bias - bias for prediction vs. measured values
# Residual Prediction Deviation (RPD)= sd of observed/RMSEP
plot(Model1)
plotXScores(Model1,show.labels = TRUE)
plotXYLoadings(Model1,show.labels =TRUE)

Model1$xloadings
Xc=scale(Xc)
eigen(var(Xc))




############
set.seed(123)
X <- matrix(rnorm(100*5), 100, 5)  # Simulated predictors
Y <- X[,1] * 2 + X[,2] * (-1) + rnorm(100)  # Response with noise

# PCA
X_scaled <- scale(X)
pca_loadings <- eigen(var(X_scaled))$vectors

# PLS
library(mdatools)
pls_model <- pls(X_scaled, Y, scale=TRUE, cv=1)
pls_loadings <- pls_model$xloadings

# Compare
print("PCA Loadings")
print(pca_loadings)
print("PLS Loadings")
print(pls_loadings)




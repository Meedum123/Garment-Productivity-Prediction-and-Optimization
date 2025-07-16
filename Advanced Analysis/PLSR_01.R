data=read.csv("train_cleandata.csv")
nrow(data)

numeric_vars <- data[,c("targeted_productivity","smv","wip",
                        "over_time","incentive","idle_time",
                        "idle_men","no_of_style_change",
                        "no_of_workers","actual_productivity")]

categorical_vars <- data[, c("date","quarter", "department", "day", "team")]  # Replace with actual categorical columns

categorical_vars <- data.frame(lapply(categorical_vars, as.factor))

# Importing the library mdatools
library(mdatools)

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

#Model Calibration Results
summary(Model1$res$cal)

#Getting model coefficients for standardize variables
summary(Model1$coeffs,ncomp = 2)

#ploting model coefficents
par(mfrow = c(2, 1))
plot(Model1$coeffs,ncomp = 2,
     type = "b", show.labels = TRUE)

plot(Model1$coeffs, ncomp = 2, type = "h", 
     alpha = 0.05,show.ci = TRUE,show.labels = TRUE)

#Getting model coefficients for original variables
show(getRegcoeffs(Model1,ncomp = 2,
                  full = TRUE, alpha = 0.05))
#########################################################
#Checking outliers
Model0= setDistanceLimits(Model1, lim.type = "ddrobust")
plotXYResiduals(Model0, show.labels = TRUE,
                labels = "indices")
#########################################################
# Variable Selection using vip score
plotVIPScores(Model1,ncomp = 2, type = "h",
              show.labels = TRUE)

# Refitting the model using selected variables
vip<- vipscores(Model1, ncomp = 2)
Model2<-pls(Xc, yc, scale = T, cv = 1, exclcols = (vip < 0.5))
summary(Model2)
plot(Model2)

#Getting model coefficents for standardize variables
summary(Model2$coeffs,ncomp = 1)

#Getting model coefficents for original variables
show(getRegcoeffs(Model2,ncomp = 1,full =T))

##########################################################
#Variable selection using P-value
exclcols<-Model1$coeffs$p.values[, 2, 1] > 0.05
show(exclcols)

# Refitting the model using the selected variables
Model3<-pls(Xc, yc, scale = TRUE, cv = 1, 
            exclcols = exclcols)
summary(Model3)
plot(Model3)

#Getting model coefficients for standardize variables
summary(Model3$coeffs,ncomp = 1)

#Getting model coefficients for original variables
show(getRegcoeffs(Model3,ncomp = 2,full = T))

#########################################################
#Predicting on Test Set
Pred<-predict(Model2, Xt,yt)
plot(Pred)
#########################################################





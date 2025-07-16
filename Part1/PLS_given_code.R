# Importing the library mdatools
library(mdatools)

#Getting people data set
data(people)
head(people)
fix(people)

# Splitting data into training and testing
set.seed(1)
idx<-sample(1: nrow(people),5) 
Xc<-people[-idx,-4]
yc<-people[-idx,4,drop = FALSE] # keep it as an array

Xt<-people[idx,-4]
yt<-people[idx,4,drop = FALSE]


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


#############################################################
#     Principal components Regression(PCR)                  #
#############################################################
library(ISLR)
library(glmnet)
library(leaps)
data("Hitters")
fix(Hitters)
dim(Hitters)
str(Hitters)
sum(is.na(Hitters$Salary)) 
Hitters =na.omit(Hitters) 
dim(Hitters)
sum(is.na(Hitters)) 

library(pls)
set.seed(2)

pcr.fit<-pcr(Salary~., data=Hitters ,scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit ,val.type="RMSEP")
pcr.fit2<-pcr(Salary~.,data=Hitters,scale=TRUE,ncomp=5) 
summary(pcr.fit2)
coef(pcr.fit2)
############################################################
#               PLSR                                       #
############################################################
set.seed(1) 
pls.fit<-plsr(Salary~., data=Hitters,scale=TRUE, 
              validation="CV")
summary(pls.fit)
validationplot(pls.fit ,val.type="RMSEP")

pls.fit2<-plsr(Salary~., data=Hitters ,scale=TRUE,ncomp=8)
summary(pls.fit2)
coef(pls.fit2)
